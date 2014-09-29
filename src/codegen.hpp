#ifndef GAMELANG_CODEGEN_HPP
#define GAMELANG_CODEGEN_HPP

#include <string>

#include "ast.hpp"

namespace gamelang
{

std::string genC(AstContext& ctx);

} // gamelang

#endif // GAMELANG_CODEGEN_HPP
