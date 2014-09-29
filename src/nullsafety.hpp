#ifndef NULLSAFETY_HPP
#define NULLSAFETY_HPP

#include <cassert>

#define NONULL(x) gamelang::detail::noNullPassesThrough(x)

namespace gamelang {
namespace detail {

template <typename T>
T noNullPassesThrough(T ptr)
{
	assert(ptr && "Illegal nullptr");
	return ptr;
}

} // detail
} // gamelang

#endif // NULLSAFETY_HPP
