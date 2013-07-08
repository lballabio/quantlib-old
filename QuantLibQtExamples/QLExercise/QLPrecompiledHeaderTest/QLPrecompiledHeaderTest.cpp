// QLPrecompiledHeaderTest.cpp : コンソール アプリケーションのエントリ ポイントを定義します。
//

#include "stdafx.h"
#include <ql/quantlib.hpp>
using namespace QuantLib;

//int _tmain(int argc, _TCHAR* argv[])
int main(int, char* [])
{
	Date today(15, July, 2012);
	std::cout << today << std::endl;


	char tmp;
	std::cin >> tmp;
	
	return 0;
}

