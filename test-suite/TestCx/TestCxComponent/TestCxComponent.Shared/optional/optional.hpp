
#pragma once

#ifdef _MSC_VER
#include <memory>

namespace std {
namespace experimental {
template<typename Type>
class optional {
public:
    explicit operator bool() const { return initialized(); }
    bool initialized()const { return nullptr != _ptr; }
	optional() {}
	optional(const Type& value) { operator = (value); }
    optional& operator = (const optional& other) {
        if (other) {
            _ptr = std::make_shared<Type>(*other);
        } else
            reset();
        return *this;
    }
    optional& operator = (const Type& value) {
        _ptr = std::make_shared<Type>(value);
        return *this;
    }
	bool operator == (const optional& value) const {
		if (initialized() != value.initialized()) return false;
		if (initialized())
			return get() == value.get();
		else
			return true;
	}
    Type *operator->() { return get_ptr(); }
    const Type *operator->()const { return get_ptr(); }
    void reset() { _ptr = nullptr; }
    Type& get() { return *_ptr; }
    const Type& get()const { return *_ptr; }
    Type *get_ptr() { return _ptr.get(); }
    Type& operator*() {return *_ptr; }
    const Type& operator*() const {return *_ptr; }
    const Type *get_ptr()const { return _ptr.get(); }
    Type value_or(const Type& v)const { return initialized() ? get() : v; }
    const Type& value()const { return *_ptr; }
    Type& value() { return *_ptr; }
private:
    std::shared_ptr<Type> _ptr;
};
}
}

#else
#include "optional_gcc.hpp"
#endif // MSVC
