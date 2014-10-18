#ifndef GAMELANG_METAPROCESSOR_HPP
#define GAMELANG_METAPROCESSOR_HPP

namespace gamelang
{

class AstContext;

/// Computes all compile-time stuff
/// Returns ast which doesn't contain any meta-constructs
/// @todo Every alias should be replaced with the thing aliased, maybe
///       This requires some changes in codegen, like not relying on
///       something being an identifier vs. actual type. Not sure if good idea.
AstContext runMetaprograms(const AstContext& input);

} // gamelang

#endif // GAMELANG_METAPROCESSOR_HPP
