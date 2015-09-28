/**
 * Copyright 2014 Dropbox, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package djinni

import djinni.ast.Record.DerivingType
import djinni.ast._
import djinni.generatorTools._
import djinni.meta._
import djinni.writer.IndentWriter

import scala.collection.mutable

class CxGenerator(spec: Spec) extends Generator(spec) {

  val cxMarshal = new CxMarshal(spec)
  val cppMarshal = new CppMarshal(spec)

  val writeCxFile = writeCppFileGeneric(spec.cxOutFolder.get, spec.cxNamespace, spec.cxFileIdentStyle, spec.cxIncludePrefix, spec.cxExt, spec.cxHeaderExt) _
  def writeHxFile(name: String, origin: String, includes: Iterable[String], fwds: Iterable[String], f: IndentWriter => Unit, f2: IndentWriter => Unit = (w => {}), namespace: Option[String] = None) =
    writeHppFileGeneric(spec.cxHeaderOutFolder.get, namespace.fold(spec.cxNamespace)(ns=>ns), spec.cxFileIdentStyle, spec.cxHeaderExt)(name, origin, includes, fwds, f, f2)

  class CxRefs(name: String) {
    var hx = mutable.TreeSet[String]()
    var hxFwds = mutable.TreeSet[String]()
    var cx = mutable.TreeSet[String]()

    def find(ty: TypeRef) { find(ty.resolved) }
    def find(tm: MExpr) {
      tm.args.foreach(find)
      find(tm.base)
    }
    def find(m: Meta) = for(r <- cxMarshal.references(m, name)) r match {
      case ImportRef(arg) => hx.add("#include " + arg)
      case DeclRef(decl, Some(spec.cxNamespace)) => hxFwds.add(decl)
      case DeclRef(_, _) =>
    }

    def findConvert(ty: TypeRef) { findConvert(ty.resolved) }
    def findConvert(tm: MExpr) {
      tm.args.foreach(find)
      findConvert(tm.base)
    }
    def findConvert(m: Meta) = for(r <- cxMarshal.convertReferences(m, name)) r match {
      case ImportRef(arg) => cx.add("#include " + arg)
    }
  }

  def writeCxFuncDecl(klass: String, method: Interface.Method, w: IndentWriter) {
    val ret = cxMarshal.returnType(method.ret)
    val params = method.params.map(p => cxMarshal.paramType(p.ty) + " " + idCx.local(p.ident))
    val constFlag = if (method.const) " const" else ""
    w.wl(s"$ret $klass::${idCx.method(method.ident)}${params.mkString("(", ", ", ")")}")
  }

  override def generateEnum(origin: String, ident: Ident, doc: Doc, e: Enum) {
    val refs = new CxRefs(ident.name)
    val self = cxMarshal.typename(ident, e)

    writeHxFile(ident, origin, refs.hx, refs.hxFwds, w => {
      w.w(s"public enum class $self").bracedSemi {
        for (o <- e.options) {
          writeDoc(w, o.doc)
          w.wl(idCx.enum(o.ident.name) + (if(o == e.options.last) "" else ","))
        }
      }
    })
  }

  def generateHxConstants(w: IndentWriter, consts: Seq[Const], self: String) = {
    def writeHxConst(w: IndentWriter, ty: TypeRef, v: Any): Unit = {
      def writeUnboxed = {
        v match {
          case l: Long => w.w(l.toString)
          case d: Double if cxMarshal.fieldType(ty) == "float" => w.w(d.toString + "f")
          case d: Double => w.w(d.toString)
          case b: Boolean => w.w(if (b) "true" else "false")
          case s: String => w.w(s)
          case e: EnumValue => w.w(cxMarshal.typename(ty) + "::" + idCx.enum(e.ty.name + "_" + e.name))
          case v: ConstRef => w.w(self + "::" + idCx.const(v))
          case z: Map[_, _] => { // Value is record
          val recordMdef = ty.resolved.base.asInstanceOf[MDef]
            val record = recordMdef.body.asInstanceOf[Record]
            val vMap = z.asInstanceOf[Map[String, Any]]
            w.wl("ref new " + cxMarshal.toCxType(ty)._1 + "(")
            w.increase()
            // Use exact sequence
            val skipFirst = SkipFirst()
            for (f <- record.fields) {
              skipFirst {w.wl(",")}
              writeHxConst(w, f.ty, vMap.apply(f.ident.name))
              w.w(" /* " + idCx.field(f.ident) + " */ ")
            }
            w.w(")")
            w.decrease()
          }
        }
      }
      ty.resolved.base match {
        case MOptional =>
        //  w.w("ref new " + cxMarshal.toCxType(ty)._1 + "(")
          writeUnboxed
       //   w.w(")")
        case _=>
          writeUnboxed
      }

    }

    val skipFirst = SkipFirst()
    for (c <- consts) {
      skipFirst{ w.wl }
      val fieldType = cxMarshal.fieldType(c.ty)
      w.w(s"property static $fieldType $self::${idCx.const(c.ident)} {$fieldType get() {return ")
      writeHxConst(w, c.ty, c.value)
      w.wl(";} } ")
    }
  }

  def generateCxConstants(w: IndentWriter, consts: Seq[Const], selfName: String) = {
    def writeCxConst(w: IndentWriter, ty: TypeRef, v: Any): Unit = v match {
      case l: Long => w.w(l.toString)
      case d: Double if cxMarshal.fieldType(ty) == "float" => w.w(d.toString + "f")
      case d: Double => w.w(d.toString)
      case b: Boolean => w.w(if (b) "true" else "false")
      case s: String => w.w(s)
      case e: EnumValue => w.w(cxMarshal.typename(ty) + "::" + idCx.enum(e.ty.name + "_" + e.name))
      case v: ConstRef => w.w(selfName + "::" + idCx.const(v))
      case z: Map[_, _] => { // Value is record
      val recordMdef = ty.resolved.base.asInstanceOf[MDef]
        val record = recordMdef.body.asInstanceOf[Record]
        val vMap = z.asInstanceOf[Map[String, Any]]
        w.wl("ref new " + cxMarshal.toCxType(ty) + "(")
        w.increase()
        // Use exact sequence
        val skipFirst = SkipFirst()
        for (f <- record.fields) {
          skipFirst {w.wl(",")}
          writeCxConst(w, f.ty, vMap.apply(f.ident.name))
          w.w(" /* " + idCx.field(f.ident) + " */ ")
        }
        w.w(")")
        w.decrease()
      }
    }

    val skipFirst = SkipFirst()
    for (c <- consts) {
      skipFirst{ w.wl }
      w.w(s"${cxMarshal.fieldType(c.ty)} const $selfName::${idCx.const(c.ident)} = ")
      writeCxConst(w, c.ty, c.value)
      w.wl(";")
    }
  }

  override def generateRecord(origin: String, ident: Ident, doc: Doc, params: Seq[TypeParam], r: Record) {
    val refs = new CxRefs(ident.name)
    r.fields.foreach(f => refs.find(f.ty))
    r.consts.foreach(c => refs.find(c.ty))

    val self = cxMarshal.typename(ident, r)
    val (cxName, cxFinal) = (ident.name, " sealed : public Platform::Object")
    val cppType = withNs(Some(spec.cppNamespace), self)

    // C++ Header
    def writeCxPrototype(w: IndentWriter) {
   //   w.wl("using namespace System;")
      writeDoc(w, doc)
      writeCxTypeParams(w, params)
      w.w("public ref class " + self + cxFinal).bracedSemi {
        w.wlOutdent("public:")
        generateHxConstants(w, r.consts, self)
        // Field definitions.
        for (f <- r.fields) {
          writeDoc(w, f.doc)
          w.wl("property " + cxMarshal.fieldType(f.ty) + " " + idCx.field(f.ident) + ";")
        }
        // Constructor.
        if(r.fields.nonEmpty) {
          w.wl
          writeAlignedCall(w, self + "(", r.fields, ")", f => cxMarshal.fieldType(f.ty) + " " + idCx.local(f.ident)).braced {
            r.fields.map(f => w.wl("this->" + idCx.field(f.ident) + " = " + idCx.local(f.ident) + ";"))
          }
        }
        w.wl(self + "() {}")

        w.wlOutdent("internal:")
        w.wl(s"$cppType toCpp();")
        w.wl(s"static $self^ fromCpp(const $cppType& value);")

        if (r.derivingTypes.contains(DerivingType.Eq)) {
          w.wl
          w.wl(s"bool Equals($self^ rhs);")
        }
        if (r.derivingTypes.contains(DerivingType.Ord)) {
          w.wl
          w.wl(s"int32 CompareTo($self^ rhs);")
        }

      }
    }
    refs.hx += cppHeader(ident.name)
    refs.cx += translationHeader()

    writeHxFile(cxName, origin, refs.hx, refs.hxFwds, writeCxPrototype)

    writeCxFile(cxName, origin, refs.cx, w => {
   //   w.wl("using namespace System;")
    //  generateCxConstants(w, r.consts, self)
      w.wl
      w.w(s"$cppType $self::toCpp()").braced {
        w.wl(s"return $cppType(")
        w.increase()
        val skipFirst = SkipFirst()
          for(f <- r.fields) {
            skipFirst{w.wl(",")}
            w.w(s"${translate(f.ty.resolved, idCx.field(f.ident))}")
          }
        w.decrease()
        w.wl(");")

      }
      w.wl
      w.w(s"$self^ $self::fromCpp(const $cppType& value)").braced {
        w.wl(s"$self^ ret = ref new $self();")
        for(f <- r.fields) {
          w.wl(s"ret->${idCx.field(f.ident)} = ${translate(f.ty.resolved, "value." + idCpp.field(f.ident))};")
        }
        w.wl("return ret;")
      }
      if (r.derivingTypes.contains(DerivingType.Eq)) {
        w.wl
        w.w(s"bool $self::Equals($self^ rhs)").braced {
          if (!r.fields.isEmpty) {
            writeAlignedCall(w, "return ", r.fields, " &&", "", f => s"this->${idCx.field(f.ident)} == rhs->${idCx.field(f.ident)}")
            w.wl(";")
          } else {
            w.wl("return true;")
          }
        }
      }
      if (r.derivingTypes.contains(DerivingType.Ord)) {
        w.wl
        w.w(s"int32 $self::CompareTo($self^ rhs)").braced {
          w.wl(s"if (rhs == nullptr) return 1;")
          w.wl("int32 tempResult = 0;")
          for (f <- r.fields) {
            def compareOrd = {
              w.wl(s"if (this->${idCx.field(f.ident)} < rhs->${idCx.field(f.ident)}) {").nested {
                w.wl(s"return -1;")
              }
              w.wl(s"} else if (rhs->${idCx.field(f.ident)} < this->${idCx.field(f.ident)}) {").nested {
                w.wl(s"tempResult = 1;")
              }
              w.wl(s"} else {").nested {
                w.wl(s"tempResult = 0;")
              }
              w.wl("}")
            }
            f.ty.resolved.base match {
              case MString => w.wl(s"tempResult = Platform::String::CompareOrdinal(this->${idCx.field(f.ident)}, rhs->${idCx.field(f.ident)});")
              case t: MPrimitive =>
                compareOrd
              case df: MDef => df.defType match {
                case DRecord => w.wl(s"tempResult = this->${idCx.field(f.ident)}->CompareTo(rhs->${idCx.field(f.ident)});")
                case DEnum => w.w(s"tempResult = this->${idCx.field(f.ident)}->CompareTo(rhs->${idCx.field(f.ident)});")
                case _ => throw new AssertionError("Unreachable")
              }
              case e: MExtern =>
                compareOrd
              case _ => throw new AssertionError("Unreachable")
            }
            w.wl("if(tempResult) return tempResult;")
          }
          w.wl("return 0;")
        }
      }
    })
  }

  override def generateInterface(origin: String, ident: Ident, doc: Doc, typeParams: Seq[TypeParam], i: Interface) {
    val refs = new CxRefs(ident.name)
    i.methods.map(m => {
      m.params.map(p => refs.find(p.ty))
      m.ret.foreach(refs.find)
    })
    i.consts.map(c => {
      refs.find(c.ty)
    })

    refs.cx = refs.hx.clone()
    i.methods.map(m => {
      m.params.map(p => refs.findConvert(p.ty))
      m.ret.foreach(refs.findConvert)
    })
 //   refs.cx.add("#include \"Marshal.h\"")

 //   val self = cxMarshal.typename(ident, i)
    val self = idCx.ty(ident.name)
    val cppSelf = cppMarshal.fqTypename(ident, i)
    if(i.ext.cpp) {
      refs.hx += "#include <memory>"
      refs.hx += cppHeader(ident.name)
      writeHxFile(ident.name, origin, refs.hx, refs.hxFwds, w=> {
        w.wl(s"public ref class $self sealed : public Platform::Object").bracedSemi {
          w.wlOutdent("public:")
          for (m <- i.methods) {
            val ret = cxMarshal.returnType(m.ret)
            val params = m.params.map(p => cxMarshal.paramType(p.ty) + " " + idCx.local(p.ident))
            if(m.static) w.w("static ")
            w.wl(s"$ret ${idCx.method(m.ident)} ${params.mkString("(", ", ", ")")};")
          }

          w.wlOutdent("internal:")
          w.wl(s"$self(std::shared_ptr<$cppSelf> cppRef) : _cppRef(cppRef) {}")
          w.wl(s"std::shared_ptr<$cppSelf> cppRef() {return _cppRef;}")
          w.wlOutdent(s"private:")
          w.wl(s"std::shared_ptr<$cppSelf> _cppRef;")
        }
      })
      refs.cx += translationHeader()
      writeCxFile(ident.name, origin, refs.cx, w=> {
        for (m <- i.methods) {
          val ret = cxMarshal.returnType(m.ret)
          val params = m.params.map(p => cxMarshal.paramType(p.ty) + " " + idCx.local(p.ident))

          w.wl(s"$ret $self::${idCx.method(m.ident)} ${params.mkString("(", ", ", ")")}").braced {
            val paramsIn = m.params.map(p=>translate(p.ty.resolved, idCx.local(p.ident))).mkString("(", ", ", ")")
            val obj = if(m.static) cppSelf + "::" else "_cppRef->"
            val call = obj + s"${idCpp.method(m.ident)}$paramsIn"
            //write ret
            w.w("try").braced {
              if(false == m.ret.isEmpty) {
                w.wl(s"auto cppRet = $call;")
                w.wl("return " + translate(m.ret.get.resolved, "cppRet") + ";")
              }
              else
                w.wl(call + ";")
            }
            w.w("catch(const std::exception& e)").braced {
              w.wl("throw ref new Platform::Exception(-1, transform<std::string, Platform::String^>()((std::string)e.what()));")
            }
          }
        }
      })
    }
    else if(i.ext.cx) {
      val isInterface = i.consts.isEmpty && (for(m <- i.methods if m.static) yield m).isEmpty
      if(false == isInterface)
        throw new AssertionError("interface to cx doesn't support non-virtual method")

      writeHxFile(ident.name, origin, refs.hx, refs.hxFwds, w=>{
        w.wl(s"public interface class $self").bracedSemi {
          w.wlOutdent("public:")
          for (m <- i.methods) {
            val ret = cxMarshal.returnType(m.ret)
            val params = m.params.map(p => cxMarshal.paramType(p.ty) + " " + idCx.local(p.ident))
            w.wl(s"virtual $ret ${idCx.method(m.ident)} ${params.mkString("(", ", ", ")")};")
          }
        }
      })

      writeCxFile(ident.name, origin, refs.cx, w=>{})

      refs.hx += "#include " + q(spec.cxIncludeCppPrefix + spec.cppFileIdentStyle(ident.name) + "." + spec.cppHeaderExt)
      refs.hx += translationHeader()
      refs.hx += cppHeader(ident.name)
      refs.hx += "#include <functional>"
      refs.hx += "#include " + q(cxMarshal.headerName(ident.name))
      writeHxFile(ident.name + "_proxy", origin, refs.hx, refs.hxFwds, w=>{
        val nativeDecls = mutable.TreeSet[String]()
        w.wl(s"template<> class CxInterfaceProxy<$cppSelf> : public $cppSelf").bracedSemi {
          w.wlOutdent("public:")
          w.wl(s"CxInterfaceProxy(${withNs(Some(spec.cxNamespace), self)}^ nativeRef)").braced {
            w.wl(s"native_call_nativeRef = [nativeRef]{ return nativeRef; };")
          }
          for(m <- i.methods) {
            val ret = m.ret.fold("void")(ty=>cppMarshal.toCppType(ty.resolved, Some(spec.cppNamespace)))
            val params = m.params.map(p => cppMarshal.paramType(p.ty) + " " + idCpp.local(p.ident))
            val methodName = idCpp.method(m.ident)
            val call = "nativeRef()->" + idCx.method(m.ident)  + m.params.map(p=>translate(p.ty.resolved, idCpp.local(p.ident), Some(spec.cxNamespace))).mkString("(", ", ", ")")

            w.wl(s"$ret $methodName(${params.mkString(", ")}) override").braced {
              if(false == m.ret.isEmpty) {
                w.wl("auto nativeRet = " + call + ";")
                w.wl("return " + translate(m.ret.get.resolved, "nativeRet", Some(spec.cxNamespace)) + ";")
              }
              else
                w.wl(call + ";")
            }
          }
          val nativeType = withNs(Some(spec.cxNamespace), idCx.ty(ident.name))
          w.wl(s"$nativeType^ nativeRef() { return native_call_nativeRef(); }")
          w.wlOutdent("private:")
          for(n <- nativeDecls)
            w.wl(n)
          w.wl(s"std::function<$nativeType^()> native_call_nativeRef;")
        }
      }, w=>Unit, Some("System"))
    }
  }

  def writeCxTypeParams(w: IndentWriter, params: Seq[TypeParam]) {
    if (params.isEmpty) return
    w.wl("template " + params.map(p => "typename " + idCx.typeParam(p.ident)).mkString("<", ", ", ">"))
  }
  def translate(ty: MExpr, name: String, cxNamespace: Option[String]=None): String = {
//    val Cpp = toCppType(ty, Some(spec.cppNamespace))
    val Cpp = cppMarshal.toCppType(ty, Some(spec.cppNamespace))
 //   val Cx = toCxType(ty, csNamespace)
    val Cx = cxMarshal.fieldType(ty, cxNamespace)
    s"transform<$Cpp, $Cx>()($name)"
  }
/*  def toCppType(ty: TypeRef, namespace: Option[String] = None): String = toCppType(ty.resolved, namespace)
  def toCppType(tm: MExpr, namespace: Option[String]): String = {
    def base(m: Meta): String = m match {
      case p: MPrimitive => p.cName
      case MString => "std::string"
      case MBinary => "std::vector<uint8_t>"
      case MOptional => spec.cppOptionalTemplate
      // case MAsync => "rc::rest::FutureHandle"
      case MList => "std::vector"
      case MSet => "std::unordered_set"
      case MMap => "std::unordered_map"
      case d: MDef =>
        d.defType match {
          case DEnum => withNs(namespace, idCpp.enumType(d.name))
          case DRecord => withNs(namespace, idCpp.ty(d.name))
          case DInterface => s"std::shared_ptr<${withNs(namespace, idCpp.ty(d.name))}>"
        }
      case p: MParam => idCpp.typeParam(p.name)
      case _ => ""
    }
    def expr(tm: MExpr): String = {
      val args = if (tm.args.isEmpty) ""
      else {
        tm.base match {
          case _ =>
            tm.args.map(expr).mkString("<", ", ", ">")
        }

      }
      base(tm.base) + args
    }
    expr(tm)
  }*/
  def cppHeader(ident: String): String = {
    //   val ext = p.ty.asInstanceOf[Interface].ext
    //   if(ext.cpp)
    "#include " + q(spec.cxIncludeCppPrefix + spec.cppFileIdentStyle(ident) + "." + spec.cppHeaderExt)
  }
  var translationGenerated = false
  def translationHeader(): String = {
    if(false == translationGenerated) {
      val hx = List[String](
        "#include " + ImportRef(spec.cppOptionalHeader).arg,
        "template<typename T> using djinni_optional = " + spec.cppOptionalTemplate + "<T>;",
        "#include " + q(spec.cxBaseLibIncludePrefix + "translate.h"),
        "using namespace System;"
      )
      writeHxFile("translation", "", hx, List[String](), w=> {})

      val cx = List[String](
        "#include " + q(spec.cxBaseLibIncludePrefix + "translate.cpp")
      )
      writeCxFile("translation", "", cx, w => {})
      translationGenerated = true
    }

    "#include " + q("translation." + spec.cxHeaderExt)
  }

}
