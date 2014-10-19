#ifndef GAMELANG_METAPROCESSOR_HPP
#define GAMELANG_METAPROCESSOR_HPP

namespace gamelang
{

class AstContext;

/// Computes all compile-time stuff
/// Returns ast which doesn't contain any meta-constructs
/// @todo Every alias should be replaced with the thing aliased, maybe.
///       Case in point: traceType gives actual type node if it's not hidden in
///       ops, and changing it to return only aliases would cause bloat.
///       Maybe minimal indirection would be good:
/// Should remove all indirection possible, including
///   - tpl'd types -> instantiated types
///   - identifers -> actual types/values
AstContext runMetaprograms(const AstContext& input);

} // gamelang

#endif // GAMELANG_METAPROCESSOR_HPP
