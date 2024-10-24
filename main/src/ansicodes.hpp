#pragma once

#include <string>
//Regular text
// #define HAVE_COLOR

#ifndef HAVE_COLOR
#define COLOR_BLK std::string("\e[0;30m")
#define COLOR_RED std::string("\e[0;31m")
#define COLOR_GRN std::string("\e[0;32m")
#define COLOR_YEL std::string("\e[0;33m")
#define COLOR_BLU std::string("\e[0;34m")
#define COLOR_MAG std::string("\e[0;35m")
#define COLOR_CYN std::string("\e[0;36m")
#define COLOR_WHT std::string("\e[0;37m")

#define COLOR_BBLK std::string("\e[1;30m")
#define COLOR_BRED std::string("\e[1;31m")
#define COLOR_BGRN std::string("\e[1;32m")
#define COLOR_BYEL std::string("\e[1;33m")
#define COLOR_BBLU std::string("\e[1;34m")
#define COLOR_BMAG std::string("\e[1;35m")
#define COLOR_BCYN std::string("\e[1;36m")
#define COLOR_BWHT std::string("\e[1;37m")
//Reset
#define COLOR_RESET std::string("\e[0m")


#else
//Regular text
#define COLOR_BLK std::string("")
#define COLOR_RED std::string("")
#define COLOR_GRN std::string("")
#define COLOR_YEL std::string("")
#define COLOR_BLU std::string("")
#define COLOR_MAG std::string("")
#define COLOR_CYN std::string("")
#define COLOR_WHT std::string("")

#define COLOR_BBLK std::string("")
#define COLOR_BRED std::string("")
#define COLOR_BGRN std::string("")
#define COLOR_BYEL std::string("")
#define COLOR_BBLU std::string("")
#define COLOR_BMAG std::string("")
#define COLOR_BCYN std::string("")
#define COLOR_BWHT std::string("")
//Reset
#define COLOR_RESET std::string("")
#endif
