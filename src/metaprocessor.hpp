#ifndef GAMELANG_METAPROCESSOR_HPP
#define GAMELANG_METAPROCESSOR_HPP

namespace gamelang
{

class AstContext;

/// Computes all compile-time stuff
/// Returns ast which doesn't contain any meta-constructs
/// @todo Every alias should be replaced with the thing aliased
AstContext runMetaprograms(const AstContext& input);

} // gamelang

#endif // GAMELANG_METAPROCESSOR_HPP
