#ifndef GAMELANG_NULLSAFETY_HPP
#define GAMELANG_NULLSAFETY_HPP

#include <cassert>

#define NONULL(x) gamelang::detail::noNullPassThrough(x)

namespace gamelang {
namespace detail {

template <typename T>
T noNullPassThrough(T ptr)
{
	assert(ptr && "Illegal nullptr");
	return ptr;
}

} // detail
} // gamelang

#endif // GAMELANG_NULLSAFETY_HPP
