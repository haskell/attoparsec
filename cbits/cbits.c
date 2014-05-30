long _atto_atomic_inc(long *ptr)
{
  return __sync_fetch_and_add(ptr, 1);
}
