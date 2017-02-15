// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from example.djinni

#include "textbox_listener.hpp"  // my header
#include "item_list.hpp"

namespace djinni_generated {

TextboxListener::TextboxListener() : ::djinni::JniInterface<::TextboxListener, TextboxListener>() {}

TextboxListener::~TextboxListener() = default;

TextboxListener::JavaProxy::JavaProxy(JniType j) : JavaProxyCacheEntry(j) { }

TextboxListener::JavaProxy::~JavaProxy() = default;

void TextboxListener::JavaProxy::update(const ::ItemList & c_items) {
    auto jniEnv = ::djinni::jniGetThreadEnv();
    ::djinni::JniLocalScope jscope(jniEnv, 10);
    const auto& data = ::djinni::JniClass<::djinni_generated::TextboxListener>::get();
    jniEnv->CallVoidMethod(getGlobalRef(), data.method_update,
                           ::djinni::get(::djinni_generated::ItemList::fromCpp(jniEnv, c_items)));
    ::djinni::jniExceptionCheck(jniEnv);
}

}  // namespace djinni_generated