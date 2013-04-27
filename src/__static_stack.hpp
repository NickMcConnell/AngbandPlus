// static_stack.hpp
// fixed-size stack of POD type

// Copyright (c) 2008 Kenneth Boyd.  This file is subject to the Boost License V1.0.

#ifndef STATIC_STACK_HPP
#define STATIC_STACK_HPP 1

#include <cstddef>
#include <cassert>

// T had better be POD
template<class T,size_t N>
class static_stack
{
protected:
	size_t stack_size;
	T stack[N];
public:

	static_stack() : stack_size(0) {};

	T& operator[](size_t i) {assert(N>i); return stack[i];};
	const T& operator[](size_t i) const {assert(N>i); return stack[i];};

	bool push();
	void delete_idx(size_t i);

	// STL glue
	size_t size() const {return stack_size;};
	static size_t max_size() {return N;};

	T& front() {return stack[0];};
	const T& front() const {return stack[0];};
	T& back() {return stack[stack_size-1];};
	const T& back() const {return stack[stack_size-1];};
};

template<class T,size_t N>
bool
static_stack<T,N>::push()
{
	if (N<=stack_size) return false;
	++stack_size;
	return true;
}

template<class T,size_t N>
void
static_stack<T,N>::delete_idx(size_t i)
{
	assert(i<stack_size);
	assert(i<N);
	--stack_size;
	if (i<stack_size) memmove(stack + i, stack + i + 1, sizeof(T)*(stack_size-i));
}


#endif
