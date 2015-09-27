
#include <cpp/cpp_exception.hpp>

namespace cppns {
	class CppExceptionImp : public CppException
	{
		int32_t throw_an_exception() override
		{
			throw std::exception("throwing exception");
		}
	};

	std::shared_ptr<CppException> CppException::get()
	{
		return std::make_shared<CppExceptionImp>();
	}
}