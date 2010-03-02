/*
 * This is a simple driver for Ryan Dahl's hand-bummed C http-parser
 * package.  It is intended to read one HTTP request after another
 * from a file, nothing more.
 *
 * For the http-parser source, see http://github.com/ry/http-parser/
 */
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "http_parser.h"

int begin(http_parser *p)
{
    return 0;
}

int url(http_parser *p, const char *at, size_t len)
{
    return 0;
}

int header_field(http_parser *p, const char *at, size_t len)
{
    return 0;
}

int header_value(http_parser *p, const char *at, size_t len)
{
    return 0;
}

int complete(http_parser *p)
{
    /* Bludgeon http_parser into understanding that we really want to
     * keep parsing after a request that in principle ought to close
     * the "connection". */
    if (!http_should_keep_alive(p)) {
	p->http_major = 1;
	p->http_minor = 1;
	p->flags &= ~6;
    }
    return 0;
}

void parse(const char *path, int fd)
{
    http_parser p;
    ssize_t nread;

    http_parser_init(&p, HTTP_REQUEST);
    p.on_message_begin = begin;
    p.on_url = url;
    p.on_header_field = header_field;
    p.on_header_value = header_value;
    p.on_message_complete = complete;

    do {
	char buf[HTTP_MAX_HEADER_SIZE];
	size_t np;

	nread = read(fd, buf, sizeof(buf));

	np = http_parser_execute(&p, buf, nread);
	if (np != nread) {
	    fprintf(stderr, "%s: parse failed\n", path);
	    break;
	}
    } while (nread > 0);
}


int main(int argc, char **argv)
{
    int i;

    for (i = 1; i < argc; i++) {
	int fd;

	fd = open(argv[i], O_RDONLY);
	if (fd == -1) {
	    perror(argv[i]);
	    continue;
	}
	parse(argv[i], fd);
	close(fd);
    }

    return 0;
}
