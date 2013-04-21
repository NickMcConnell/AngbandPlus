
typedef unsigned long cmucl_lispobj;

cmucl_lispobj
funcall0(cmucl_lispobj function) {
    if (function)
	return 0;
    else
	return 0;
}

cmucl_lispobj
funcall2(cmucl_lispobj function, cmucl_lispobj arg1, cmucl_lispobj arg2) {
    if (function || arg1 || arg2)
	return 0;
    else
	return 0;
}

cmucl_lispobj
funcall3(cmucl_lispobj function, cmucl_lispobj arg1, cmucl_lispobj arg2, cmucl_lispobj arg3) {
    if (function || arg1 || arg2 || arg3)
	return 0;
    else
	return 0;
}
