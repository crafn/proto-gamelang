#include <cassert>
#include <cstdio>
#include <iostream>
#include <string>
#include <vector>

namespace gamelang
{

bool whitespace(char ch)
{ return ch == ' ' || ch == '\t' || ch == '\n'; }

bool tokenSeparator(char ch)
{
	switch (ch) {
		case ';':
		case '(':
		case ')':
		case '{':
		case '}':
			return true;
		default: return false;	
	}
}

using Tokens= std::vector<std::string>;

Tokens tokenize(const char* filepath)
{
	Tokens tokens;
	char* contents= nullptr;
	std::size_t contents_size= 0;

	FILE* file= std::fopen(filepath, "rb");
	if (!file)
		goto cleanup;
	{ // Read file
		std::fseek(file, 0, SEEK_END);
		long size= std::ftell(file);
		std::fseek(file, 0, SEEK_SET);

		contents= static_cast<char*>(std::malloc(size + 1));
		int count= std::fread(contents, size, 1, file);
		assert(count == 1);
		contents_size= size;
	}
	{ // Tokenize	
		char* cur= contents;
		char* tok_start= contents;
		char const* end= contents + contents_size;
		while (cur < end) {
			char ch= *cur;
			if (whitespace(ch)) {
				if (cur > tok_start)
					tokens.emplace_back(tok_start, cur);
				tok_start= cur + 1;
			} else if (tokenSeparator(ch)) {
				if (cur > tok_start)
					tokens.emplace_back(tok_start, cur);
				tok_start= cur;	
			}
			++cur;
		}
	}
	cleanup:
	std::fclose(file);
	std::free(contents);
	
	return tokens;
}

}

int main(int argc, const char* argv[])
{
	if (argc <= 1)
		return 0;

	const char* filepath= argv[1];
	std::cout << "file: " << filepath << std::endl;
	for (auto&& token : gamelang::tokenize(filepath)) {
		std::cout << token << std::endl;
	}
}
