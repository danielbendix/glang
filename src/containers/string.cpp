#include "containers/string.h"

char emptyStringData[1] = {'\0'};

String String::empty = String{emptyStringData, 0};
