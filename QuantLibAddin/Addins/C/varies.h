
/*
	this code is based on the article at
	http://www.kuro5hin.org/story/2002/5/1/142321/9513

	I've emailed the author to ask for his requirements
	regarding copyright / attribution

*/

#ifndef varies_h
#define varies_h

typedef enum { INT, LONG, DOUBLE, CHARP } Type;

typedef struct {
	union {
		int AsInt;
		long AsLong;
		double AsDouble;
		char* AsCharP;
	};
	Type type;
	char* Label;
} Varies;

typedef struct {
	int count;
	Varies *varies;
} VariesList;

const char *variesToString(const Varies *v);
void freeVariesList(VariesList *vl);

#endif
