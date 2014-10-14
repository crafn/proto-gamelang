#ifndef GAMELANG_METAPROCESSOR_HPP
#define GAMELANG_METAPROCESSOR_HPP

namespace gamelang
{

class AstContext;

/// Computes all compile-time stuff
/// Returns ast which doesn't contain any meta-constructs
/// @todo Returned ast is kind of broken (missing at least id bindings)
AstContext runMetaprograms(const AstContext& input);

} // gamelang

#endif // GAMELANG_METAPROCESSOR_HPP
