CC := gcc
CFLAGS := -O3
CPPFLAGS := -I$(HOME)/hg/http-parser

c-rfc: rfc2616.o http_parser.o
	$(CC) -o $@ $^

vpath %.c $(HOME)/hg/http-parser
