#ifndef GAMELANG_METAPROCESSOR_HPP
#define GAMELANG_METAPROCESSOR_HPP

namespace gamelang
{

class AstContext;

/// Returns Ast which doesn't contain any meta-constructs
/// @todo Returned Ast is kind of broken (missing at least id bindings)
AstContext runMetaprograms(const AstContext& input);

} // gamelang

#endif // GAMELANG_METAPROCESSOR_HPP
