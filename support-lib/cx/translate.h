#pragma once
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <collection.h>

namespace wfc = Windows::Foundation::Collections;

namespace System {
	template<typename Cpp>
	struct determine_transformation_type;

	template<typename Cpp, class Cx>
	struct transform
	{
		Cpp operator()(Cx cx)
		{
			Cpp cpp;
			determine_transformation_type<Cpp>::type()(cx, cpp);
			return cpp;
		}
		Cx operator()(const Cpp& cpp)
		{
			Cx cx;
			determine_transformation_type<Cpp>::type()(cpp, cx);
			return cx;
		}
	};
	template<typename Type>
	struct transform<Type, Type>
	{
		Type operator()(Type value) { return value; }
	};

	//////////////////////////////////////////////////////////////////////////////
	//determinators
	//////////////////////////////////////////////////////////////////////////////
	template<typename Cpp> struct is_primitive { static const bool value = false; };
	template<> struct is_primitive<bool> { static const bool value = true; };
	template<> struct is_primitive<int8_t> { static const bool value = true; };
	template<> struct is_primitive<int16_t> { static const bool value = true; };
	template<> struct is_primitive<int32_t> { static const bool value = true; };
	template<> struct is_primitive<int64_t> { static const bool value = true; };
	template<> struct is_primitive<double> { static const bool value = true; };
	template<> struct is_primitive<std::string> { static const bool value = true; };
	template<> struct is_primitive<std::vector<uint8_t>> { static const bool value = true; };

	template<typename Cpp> struct is_optional { static const bool value = false; };
	template<typename Type> struct is_optional<djinni_optional<Type>> { static const bool value = true; };

	template<typename Cpp> struct is_vector { static const bool value = false; };
	template<typename Type> struct is_vector<std::vector<Type>> { static const bool value = true; };

	template<typename Cpp> struct is_set { static const bool value = false; };
	template<typename Type> struct is_set<std::unordered_set<Type>> { static const bool value = true; };

	template<typename Cpp> struct is_map { static const bool value = false; };
	template<typename Key, typename Value> struct is_map<std::unordered_map<Key, Value>> { static const bool value = true; };

	template<typename Cpp> struct is_interface { static const bool value = false; };
	template<typename Type> struct is_interface<std::shared_ptr<Type>> { static const bool value = true; };
	//////////////////////////////////////////////////////////////////////////////
	//translators
	//////////////////////////////////////////////////////////////////////////////
	struct translate_primitive
	{
		void operator()(const std::string& cpp, Platform::String^& cx);
		void operator()(Platform::String^ cx, std::string& cpp);

		void operator()(int8_t cpp, unsigned char& cx) { cx = (unsigned char)cpp; }
		void operator()(unsigned char cx, int8_t& cpp) { cpp = (int8_t)cx; }

		void operator()(bool c1, bool& c2) { c2 = c1; }
		void operator()(int16_t c1, int16_t& c2) { c2 = c1; }
		void operator()(int32_t c1, int32_t& c2) { c2 = c1; }
		void operator()(int64_t c1, int64_t& c2) { c2 = c1; }
		void operator()(double c1, double& c2) { c2 = c1; }

		void operator()(const std::vector<uint8_t>& cpp, Platform::Array<uint8_t>^& cx)
		{
			cx = ref new Platform::Array<uint8_t>(cpp.size());
			memcpy(cx->Data, cpp.data(), cpp.size());
		}
		void operator()(const Platform::Array<uint8_t>^ cx, std::vector<uint8_t>& cpp)
		{
			if (nullptr == cx)
			{
				cpp.clear();
				return;
			}
			cpp.resize(cx->Length);
			memcpy(cpp.data(), cx->Data, cpp.size());
		}
	};

	struct translate_optional
	{
		void operator()(const djinni_optional<bool>& cpp, BoolRef^& cx) { cx = cpp ? ref new BoolRef(cpp.get()) : nullptr; }
		void operator()(const djinni_optional<uint8_t>& cpp, ByteRef^& cx) { cx = cpp ? ref new ByteRef(cpp.get()) : nullptr; }
		void operator()(const djinni_optional<int16_t>& cpp, ShortRef^& cx) { cx = cpp ? ref new ShortRef(cpp.get()) : nullptr; }
		void operator()(const djinni_optional<int32_t>& cpp, IntRef^& cx) { cx = cpp ? ref new IntRef(cpp.get()) : nullptr; }
		void operator()(const djinni_optional<int64_t>& cpp, LongRef^& cx) { cx = cpp ? ref new LongRef(cpp.get()) : nullptr; }
		void operator()(const djinni_optional<float>& cpp, FloatRef^& cx) { cx = cpp ? ref new FloatRef(cpp.get()) : nullptr; }
		void operator()(const djinni_optional<double>& cpp, DoubleRef^& cx) { cx = cpp ? ref new DoubleRef(cpp.get()) : nullptr; }
		void operator()(const djinni_optional<std::string>& cpp, StringRef^& cx)
		{
			cx = cpp ? ref new StringRef(transform<std::string, Platform::String^>()(cpp.get())) : nullptr;
		}
		template<typename Cpp, class Cx>
		void operator()(const djinni_optional<Cpp>& cpp, Cx^& cx)
		{
			if (cpp)
				determine_transformation_type<Cpp>::type()(cpp.get(), cx);
			else
				cx = nullptr;
		}

		void operator()(BoolRef^ cx, djinni_optional<bool>& cpp) { cpp = cx ? cx->Value : std::remove_reference<decltype(cpp)>::type(); }
		void operator()(ByteRef^ cx, djinni_optional<uint8_t>& cpp) { cpp = cx ? cx->Value : std::remove_reference<decltype(cpp)>::type(); }
		void operator()(ShortRef^ cx, djinni_optional<int16_t>& cpp) { cpp = cx ? cx->Value : std::remove_reference<decltype(cpp)>::type(); }
		void operator()(IntRef^ cx, djinni_optional<int32_t>& cpp) { cpp = cx ? cx->Value : std::remove_reference<decltype(cpp)>::type(); }
		void operator()(LongRef^ cx, djinni_optional<int64_t>& cpp) { cpp = cx ? cx->Value : std::remove_reference<decltype(cpp)>::type(); }
		void operator()(FloatRef^ cx, djinni_optional<float>& cpp) { cpp = cx ? cx->Value : std::remove_reference<decltype(cpp)>::type(); }
		void operator()(DoubleRef^ cx, djinni_optional<double>& cpp) { cpp = cx ? cx->Value : std::remove_reference<decltype(cpp)>::type(); }
		void operator()(StringRef^ cx, djinni_optional<std::string>& cpp)
		{
			cpp = cx ? transform<std::string, Platform::String^>()(cx->Value) : std::remove_reference<decltype(cpp)>::type();
		}
		template<typename Cpp, class Cx>
		void operator()(Cx^ cx, djinni_optional<Cpp>& cpp)
		{
			if (cx)
				cpp = transform<Cpp, Cx^>()(cx);
			else
				cpp = std::remove_reference<decltype(cpp)>::type();
		}
	};

	struct translate_vector
	{
		template<typename Cpp, class Cx>
		void operator()(const std::vector<Cpp>& cpp, Windows::Foundation::Collections::IVector<Cx>^& cx)
		{
			cx = ref new Platform::Collections::Vector<Cx>();
			for (auto& c : cpp)
				cx->Append(transform<Cpp, Cx>()(c));
		}
		template<typename Cpp, class Cx>
		void operator()(Windows::Foundation::Collections::IVector<Cx>^ cx, std::vector<Cpp>& cpp)
		{
			cpp.clear();
			if (nullptr == cx)
				return;
			for (auto& c : cx)
				cpp.push_back(transform<Cpp, Cx>()(c));
		}
	};

	struct translate_set
	{
		template<typename Cpp, class Cx>
		void operator()(const std::unordered_set<Cpp>& cpp, wfc::IIterable<Cx>^& cx)
		{
			auto cxs = ref new Platform::Collections::Vector<Cx>();
			for (auto& c : cpp)
				cxs->Append(transform<Cpp, Cx>()(c));
			cx = cxs;
		}
		template<typename Cpp, class Cx>
		void operator()(wfc::IIterable<Cx>^ cx, std::unordered_set<Cpp>& cpp)
		{
			cpp.clear();
			if (nullptr == cx)
				return;
			for (auto& c : cx)
				cpp.insert(transform<Cpp, Cx>()(c));
		}
	};

	struct translate_map
	{
		template<typename CppKey, typename CppValue, class CxKey, class CxValue>
		void operator()(const std::unordered_map<CppKey, CppValue>& cpp, wfc::IMap<CxKey, CxValue>^& cx)
		{
			cx = ref new Platform::Collections::UnorderedMap<CxKey, CxValue>();
			for (auto& c : cpp)
			{
				CxKey key = transform<CppKey, CxKey>()(c.first);
				CxValue value = transform<CppValue, CxValue>()(c.second);
				cx->Insert(key, value);
			}
		}
		template<typename CppKey, typename CppValue, class CxKey, class CxValue>
		void operator()(wfc::IMap<CxKey, CxValue>^ cx, std::unordered_map<CppKey, CppValue>& cpp)
		{
			cpp.clear();
			if (nullptr == cx)
				return;
			for (auto& c : cx)
			{
				CppKey key = transform<CppKey, CxKey>()(c->Key);
				CppValue value = transform<CppValue, CxValue>()(c->Value);
				cpp[key] = value;
			}
		}
	};

	template<typename Type>
	class CxInterfaceProxy;
	template<typename Cpp>
	struct is_cx_implemented { static const bool value = std::is_convertible<CxInterfaceProxy<Cpp>, Cpp>::value; };

	struct translate_interface
	{
		template<typename Cpp, class Cx>
		void operator()(const std::shared_ptr<Cpp>& cpp, Cx^& cx)
		{
			std::conditional<is_cx_implemented<Cpp>::value,
				translate_interface_cx_implemented,
				translate_interface_cpp_implemented>::type()(cpp, cx);
		}
		template<typename Cpp, class Cx>
		void operator()(Cx^ cx, std::shared_ptr<Cpp>& cpp)
		{
			std::conditional<is_cx_implemented<Cpp>::value,
				translate_interface_cx_implemented,
				translate_interface_cpp_implemented>::type()(cx, cpp);
		}
	};
	struct translate_interface_cx_implemented
	{
		template<typename Cpp, class Cx>
		void operator()(const std::shared_ptr<Cpp>& cpp, Cx^& cx)
		{
			cx = cpp ? std::static_pointer_cast<CxInterfaceProxy<Cpp>, Cpp>(cpp)->nativeRef() : nullptr;
		}
		template<typename Cpp, class Cx>
		void operator()(Cx^ cx, std::shared_ptr<Cpp>& cpp) { cpp = cx ? std::make_shared<CxInterfaceProxy<Cpp>>(cx) : nullptr; }
	};
	struct translate_interface_cpp_implemented
	{
		template<typename Cpp, class Cx>
		void operator()(const std::shared_ptr<Cpp>& cpp, Cx^& cx)
		{
			cx = cpp ? ref new Cx(cpp) : nullptr;
		}
		template<typename Cpp, class Cx>
		void operator()(Cx^ cx, std::shared_ptr<Cpp>& cpp) { cpp = cx ? cx->cppRef() : nullptr; }
	};
	struct translate_enum
	{
		template<class C1, class C2>
		void operator()(C1 c1, C2& c2) { c2 = C2(c1); }
	};
	struct translate_record
	{
		template<typename Cpp, class Cx>
		void operator()(const Cpp& cpp, Cx^& cx) { cx = Cx::fromCpp(cpp); }
		template<typename Cpp, class Cx>
		void operator()(Cx^ cx, Cpp& cpp) { cpp = cx->toCpp(); }
	};
	//////////////////////////////////////////////////////////////////////////////
	//translation entry
	//////////////////////////////////////////////////////////////////////////////
	template<typename Cpp>
	struct determine_transformation_type
	{
		typedef
			typename std::conditional < is_primitive<Cpp>::value, translate_primitive,
			typename std::conditional < is_optional<Cpp>::value, translate_optional,
			typename std::conditional < is_vector<Cpp>::value, translate_vector,
			typename std::conditional < is_set<Cpp>::value, translate_set,
			typename std::conditional < is_map<Cpp>::value, translate_map,
			typename std::conditional < is_interface<Cpp>::value, translate_interface,
			typename std::conditional < std::is_enum<Cpp>::value, translate_enum,
			translate_record
			>::type>::type>::type>::type>::type>::type>::type
			type;
	};

	template<>
	struct transform<std::vector<uint8_t>, Platform::Array<uint8_t>^>
	{
		std::vector<uint8_t> operator()(const Platform::Array<uint8_t>^ cx)
		{
			std::vector<uint8_t> cpp;
			determine_transformation_type<std::vector<uint8_t>>::type()(cx, cpp);
			return cpp;
		}
		Platform::Array<uint8_t>^ operator()(const std::vector<uint8_t>& cpp)
		{
			Platform::Array<uint8_t>^ cx;
			determine_transformation_type<std::vector<uint8_t>>::type()(cpp, cx);
			return cx;
		}
	};
}
