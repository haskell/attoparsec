/*
 * This is a simple driver for Ryan Dahl's hand-bummed C http-parser
 * package.  It is intended to read one HTTP request after another
 * from a file, nothing more.
 *
 * For "feature parity" with the Haskell code in RFC2616.hs, we
 * allocate and populate a simple structure describing each request,
 * since that's the sort of thing that many real applications would
 * themselves do and the library doesn't do this for us.
 *
 * For the http-parser source, see http://github.com/ry/http-parser/
 */

/*
 * Turn off this preprocessor symbol to have the callbacks do nothing
 * at all, which "improves performance" by about 50%.
 */
#define LOOK_BUSY

#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "http_parser.h"

struct http_string {
    size_t len;
    char value[0];
};
    
struct http_header {
    struct http_string *name;
    struct http_string *value;
    struct http_header *next;
};
    
struct http_request {
    struct http_string *method;
    struct http_string *uri;
    struct http_header *headers, *last;
};
  
static void *xmalloc(size_t size)
{
    void *ptr;

    if ((ptr = malloc(size)) == NULL) {
	perror("malloc");
	exit(1);
    }

    return ptr;
}

static struct http_string *xstrdup(const char *src, size_t len, size_t extra)
{
    struct http_string *dst = xmalloc(sizeof(*dst) + len + extra);
    memcpy(dst->value, src, len);
    dst->len = len;
    return dst;
}

static void xstrcat(struct http_string **dst, const char *src, size_t len)
{
    struct http_string *p;

    if (*dst == NULL) {
	*dst = xstrdup(src, len, 0);
	return;
    }
    
    p = xstrdup((*dst)->value, (*dst)->len, len);
    memcpy(p->value + (*dst)->len, src, len);
    p->len += len;
    free(*dst);
    *dst = p;
}

static int begin(http_parser *p)
{
#ifdef LOOK_BUSY
    struct http_request *req = xmalloc(sizeof(*req));

    req->method = NULL;
    req->uri = NULL;
    req->headers = NULL;
    req->last = NULL;

    p->data = req;
#endif

    return 0;
}

static int url(http_parser *p, const char *at, size_t len)
{
#ifdef LOOK_BUSY
    struct http_request *req = p->data;    

    xstrcat(&req->uri, at, len);
#endif

    return 0;
}

static int header_field(http_parser *p, const char *at, size_t len)
{
#ifdef LOOK_BUSY
    struct http_request *req = p->data;

    if (req->last && req->last->value == NULL) {
	xstrcat(&req->last->name, at, len);
    } else {
	struct http_header *hdr = xmalloc(sizeof(*hdr));

	hdr->name = xstrdup(at, len, 0);
	hdr->value = NULL;
	hdr->next = NULL;
    
	if (req->last)
	    req->last->next = hdr;
	req->last = hdr;
	if (req->headers == NULL)
	    req->headers = hdr;
    }
#endif

    return 0;
}

static int header_value(http_parser *p, const char *at, size_t len)
{
#ifdef LOOK_BUSY
    struct http_request *req = p->data;

    xstrcat(&req->last->value, at, len);
#endif

    return 0;
}

static int complete(http_parser *p)
{
#ifdef LOOK_BUSY
    struct http_request *req = p->data;
    struct http_header *hdr, *next;

    free(req->method);
    free(req->uri);
	
    for (hdr = req->headers; hdr != NULL; hdr = next) {
	next = hdr->next;
	free(hdr->name);
	free(hdr->value);
	free(hdr);
	hdr = next;
    }

    free(req);
#endif
    
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

static void parse(const char *path, int fd)
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

/*
 * Local Variables:
 * c-file-style: "stroustrup"
 * End:
 */
