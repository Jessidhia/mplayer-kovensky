/*
 * This file is part of MPlayer.
 *
 * MPlayer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * MPlayer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with MPlayer; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef MPLAYER_BSTR_H
#define MPLAYER_BSTR_H

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

#include "talloc.h"

/* NOTE: 'len' is size_t, but most string-handling functions below assume
 * that input size has been sanity checked and len fits in an int.
 */
struct bstr {
    unsigned char *start;
    size_t len;
};

// demux_rtp.cpp (live555) C++ compilation workaround
#ifndef __cplusplus
static inline char *bstrdup0(void *talloc_ctx, struct bstr str)
{
    return talloc_strndup(talloc_ctx, (char *)str.start, str.len);
}

static inline struct bstr bstrdup(void *talloc_ctx, struct bstr str)
{
    struct bstr r = { talloc_strndup(talloc_ctx, str.start, str.len), str.len };
    return r;
}

static inline struct bstr bstr(const unsigned char *s)
{
    return (struct bstr){(unsigned char *)s, s ? strlen(s) : 0};
}

int bstrcmp(struct bstr str1, struct bstr str2);
int bstrcasecmp(struct bstr str1, struct bstr str2);
int bstrchr(struct bstr str, int c);
int bstrrchr(struct bstr str, int c);
int bstrcspn(struct bstr str, const char *reject);

int bstr_find(struct bstr haystack, struct bstr needle);
struct bstr *bstr_splitlines(void *talloc_ctx, struct bstr str);
struct bstr bstr_lstrip(struct bstr str);
struct bstr bstr_strip(struct bstr str);
struct bstr bstr_split(struct bstr str, const char *sep, struct bstr *rest);
struct bstr bstr_splice(struct bstr str, int start, int end);
long long bstrtoll(struct bstr str, struct bstr *rest, int base);
double bstrtod(struct bstr str, struct bstr *rest);
void bstr_lower(struct bstr str);
int bstr_sscanf(struct bstr str, const char *format, ...);

// Decode the UTF-8 code point at the start of the string,, and return the
// character.
// After calling this function, *out_next will point to the next character.
// out_next can be NULL.
// On error, -1 is returned, and *out_next is not modified.
int bstr_decode_utf8(struct bstr str, struct bstr *out_next);

// Return the length of the UTF-8 sequence that starts with the given byte.
// Given a string char *s, the next UTF-8 code point is to be expected at
//      s + bstr_parse_utf8_code_length(s[0])
// On error, -1 is returned. On success, it returns a value in the range [1, 4].
int bstr_parse_utf8_code_length(unsigned char b);

// Return the text before the next line break, and return it. Change *rest to
// point to the text following this line break. (rest can be NULL.)
// Unlike bstr_splitlines, possible \r characters coming from files with CR+LF
// line breaks are stripped.
struct bstr bstr_getline(struct bstr str, struct bstr *rest);

bool bstr_case_startswith(struct bstr s, struct bstr prefix);
bool bstr_case_endswith(struct bstr s, struct bstr suffix);
struct bstr bstr_strip_ext(struct bstr str);
struct bstr bstr_get_ext(struct bstr s);


static inline struct bstr bstr_cut(struct bstr str, int n)
{
    if (n < 0) {
        n += str.len;
        if (n < 0)
            n = 0;
    }
    if (n > str.len)
        n = str.len;
    return (struct bstr){str.start + n, str.len - n};
}

static inline bool bstr_startswith(struct bstr str, struct bstr prefix)
{
    if (str.len < prefix.len)
        return false;
    return !memcmp(str.start, prefix.start, prefix.len);
}

static inline bool bstr_startswith0(struct bstr str, const char *prefix)
{
    return bstr_startswith(str, bstr(prefix));
}

static inline bool bstr_endswith(struct bstr str, struct bstr suffix)
{
    if (str.len < suffix.len)
        return false;
    return !memcmp(str.start + str.len - suffix.len, suffix.start, suffix.len);
}

static inline bool bstr_endswith0(struct bstr str, const char *suffix)
{
    return bstr_endswith(str, bstr(suffix));
}

static inline int bstrcmp0(struct bstr str1, const char *str2)
{
    return bstrcmp(str1, bstr(str2));
}

static inline int bstrcasecmp0(struct bstr str1, const char *str2)
{
    return bstrcasecmp(str1, bstr(str2));
}

static inline int bstr_find0(struct bstr haystack, const char *needle)
{
    return bstr_find(haystack, bstr(needle));
}

#endif

// Create bstr compound literal from null-terminated string
#define BSTR(s) (struct bstr){(char *)(s), (s) ? strlen(s) : 0}
// create a pair (not single value!) for "%.*s" printf syntax
#define BSTR_P(bstr) (int)((bstr).len), (bstr).start

#define WHITESPACE " \f\n\r\t\v"

#endif /* MPLAYER_BSTR_H */
