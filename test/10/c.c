
#include <assert.h>

struct S1 {
	char i1;
	float i2;
};

struct S {
	int i1;
	char i2;
	struct S1 s1;
};

struct S c_test(int i)
{
	assert(i == 1);
	struct S s;
	s.i1 = 1;
	s.i2 = 2;
	s.s1.i1 = 3;
	s.s1.i2 = 1.5;
	return s;
}
