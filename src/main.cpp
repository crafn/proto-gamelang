#include <cstdio>
#include <iostream>
#include <string>
#include <vector>

namespace gamelang
{

using Tokens= std::vector<std::string>>;

Tokens tokenize(const char* filepath)
{
	FILE* file= std::fopen(filepath, "rb");
	if (!file)
		goto cleanup;
	std::fseek(file, 0, SEEK_END);
	long size= std::ftell(file);
	std::fseek(file, 0, SEEK_SET);

	char* contents= std::malloc(size + 1);
	long contents_size= std::fread(string, size, 1, file);
	assert(contents == contents_size);
	
	Tokens tokens;
	char* begin= contents;
	char* end= contents + contents_size;
	std::string buf;
	while (begin < end) {
		char ch= *begin;
		if (ch == ' ' || ch == '\t' || ch == '\n') {
			if (!buf.empty())
				tokens.push_back(buf);
			buf.clear();
		} else {
			buf.append(ch);
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
		return;

	for (auto&& token : tokenize(argv[0])) {
		std::cout << token << std::endl;
	}
}
