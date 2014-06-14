
#include <assert.h>

struct S1 {
	int j1;
	int j2;
};

struct S {
	int i1;
	int i2;
	struct S1 s1;
	int i3;
	int i4;
};

struct S c_test(int i)
{
	assert(i == 1);
	struct S s;
	s.i1 = 1;
	s.i2 = 2;
	s.i3 = 3;
	s.i4 = 4;
	s.s1.j1 = 5;
	s.s1.j2 = 6;
	return s;
}
