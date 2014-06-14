
#include <assert.h>

struct S1 {
	char c;
	int i;
};

struct S {
	float i;
	struct S1* s1;
};

static struct S1 s1;

struct S c_test(int i)
{
	assert(i == 1);
	struct S s;
	s.i = (float)1;
	s.s1 = &s1;
	s.s1->c = 2;
	s.s1->i = 3;
	return s;
}
