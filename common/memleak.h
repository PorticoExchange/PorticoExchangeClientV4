#ifndef LIGHTNING_COMMON_MEMLEAK_H
#define LIGHTNING_COMMON_MEMLEAK_H
#include "config.h"
#include <ccan/strmap/strmap.h>
#include <ccan/tal/tal.h>
#include <inttypes.h>

struct htable;
struct list_head;

/**
 * memleak_init: Initialize memleak detection; you call this at the start!
 *
 * notleak() won't have an effect if called before this (but naming
 * tal objects with suffix _notleak works).
 */
void memleak_init(void);

/**
 * notleak: remove a false-positive tal object.
 * @p: the tal allocation.
 *
 * This marks a tal pointer (and anything it refers to) as not being
 * leaked.  Think hard before using this!
 */
#define notleak(p) ((memleak_typeof(p))notleak_((p), false))

/* Mark a pointer and all its tal children as not being leaked.
 * You don't want this; it's for simplifying handling of the incoming
 * command which asks lightningd to do the dev check. */
#define notleak_with_children(p) ((memleak_typeof(p))notleak_((p), true))

#if HAVE_TYPEOF
#define memleak_typeof(var) typeof(var)
#else
#define memleak_typeof(var) void *
#endif /* !HAVE_TYPEOF */

void *notleak_(void *ptr, bool plus_children);

#if DEVELOPER
/**
 * memleak_add_helper: help memleak look inside this tal object
 * @p: the tal object
 * @cb: the callback.
 *
 * memleak looks for tal pointers inside a tal object memory, but some
 * structures which use bit-stealing on pointers or use non-tal allocations
 * will need this.
 *
 * The callback usually calls memleak_remove_*.
 */
#define memleak_add_helper(p, cb)					\
	memleak_add_helper_((p),					\
			    typesafe_cb_preargs(void, const tal_t *,	\
						(cb), (p),		\
						struct htable *))
#else
/* Don't refer to cb at all if !DEVELOPER */
#define memleak_add_helper(p, cb)
#endif

/* For update-mock: memleak_add_helper_ mock empty */
void memleak_add_helper_(const tal_t *p, void (*cb)(struct htable *memtable,
						    const tal_t *));

/**
 * memleak_start:  allocate a htable with all tal objects
 * @ctx: the context to allocate the htable from
 */
struct htable *memleak_start(const tal_t *ctx);

/**
 * memleak_ptr: this pointer is not a memleak.
 * @memtable: the memtable create by memleak_start.
 * @p: the pointer.
 *
 * This tells memleak that @p (a tal allocation) is not a leak.  Returns
 * true if it was in the memleak table (it will no longer be).
 */
bool memleak_ptr(struct htable *memtable, const void *p);

/**
 * memleak_scan_obj - this tal object and anything it references are not leaks.
 * @memtable: the memtable create by memleak_start.
 * @obj: the tal pointer
 *
 * This removes @obj from the memtable, then looks for any tal pointers
 * inside @obj and calls memleak_scan_obj() on those if not already removed.
 */
void memleak_scan_obj(struct htable *memtable, const void *obj);

/**
 * memleak_scan_list_head - this list is not a leak.
 * @memtable: the memtable create by memleak_start.
 * @l: the list_head pointer
 *
 * This removes @l from the memtable, and any elements in the list.  Usually
 * used for file-scope linked lists.
 */
void memleak_scan_list_head(struct htable *memtable, const struct list_head *l);

/**
 * memleak_scan_region - scan a non-tal allocation for references.
 * @memtable: the memtable create by memleak_start.
 * @p: the tal pointer
 * @len: the length in bytes.
 *
 * Sometimes we have a stack or file-scope object which contains pointers.
 */
void memleak_scan_region(struct htable *memtable, const void *p, size_t len);

/* Objects inside this htable (which is opaque to memleak) are not leaks. */
void memleak_scan_htable(struct htable *memtable, const struct htable *ht);

/* Objects inside this uintmap (which is opaque to memleak) are not leaks. */
#define memleak_scan_uintmap(memtable, umap)		\
	memleak_scan_intmap_(memtable, uintmap_unwrap_(umap))

struct intmap;
void memleak_scan_intmap_(struct htable *memtable, const struct intmap *m);

/* Objects inside this strmap (which is opaque to memleak) are not leaks. */
#define memleak_scan_strmap(memtable, strmap) \
	memleak_scan_strmap_((memtable), tcon_unwrap(strmap))
void memleak_scan_strmap_(struct htable *memtable, const struct strmap *m);

/**
 * memleak_ignore_children - ignore all this tal object's children.
 * @memtable: the memtable created by memleak_start
 * @p: the tal pointer.
 *
 * This is equivalent to calling memleak_ptr() on every child of @p
 * recursively.  This is a big hammer, so be careful!
 */
void memleak_ignore_children(struct htable *memtable, const void *p);

/**
 * memleak_get: get (and remove) a leak from memtable, or NULL
 * @memtable: the memtable after all known allocations removed.
 * @backtrace: the backtrace to set if there is one.
 *
 * If this returns NULL, it means the @memtable was empty.  Otherwise
 * it return a pointer to a leak (and removes it from @memtable)
 */
const void *memleak_get(struct htable *memtable, const uintptr_t **backtrace);

extern struct backtrace_state *backtrace_state;

#if DEVELOPER
/* Only defined if DEVELOPER */
bool dump_memleak(struct htable *memtable,
		  void PRINTF_FMT(1,2) (*print)(const char *fmt, ...));
#endif

#endif /* LIGHTNING_COMMON_MEMLEAK_H */
