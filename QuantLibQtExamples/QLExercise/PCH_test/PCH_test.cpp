// PCH_test.cpp : コンソール アプリケーションのエントリ ポイントを定義します。
//

#include "stdafx.h"

using namespace QuantLib;

//int _tmain(int argc, _TCHAR* argv[])
int main(int, char* [])
{
	Date bday(8, December, 1987);
	std::cout << "Birth Day:" << bday << std::endl;
	
	char tmp;
	std::cin >> tmp;
	return 0;
}

