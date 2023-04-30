extern int g_debug;

size_t
__mesabi_uldiv (size_t a, size_t b, size_t *remainder)
{
  remainder[0] = a % b;
  return a / b;
}

char *
ntoab_local (long x, unsigned base, int signed_p)
{
  char* __itoa_buf = "012345678911234567890";
  if (g_debug > 2) eputs ("ntoab 00\n");
  char *p = __itoa_buf + 11;
  if (g_debug > 2) eputs ("ntoab 03\n");

  p[0] = 0;
  if (g_debug > 2) eputs ("ntoab 04\n");
  p = p - 1;
  if (g_debug > 2) eputs ("ntoab 05\n");

  int sign_p = 0;
  size_t i;
  size_t u;
  size_t b = base;
  if (signed_p != 0 && x < 0)
    {
      sign_p = 1;
      /* Avoid LONG_MIN */
      u = (-(x + 1));
      u = u + 1;
    }
  else
    u = x;

  do
    {
      u = __mesabi_uldiv (u, b, &i);
      if (i > 9)
        p[0] = 'a' + i - 10;
      else
        p[0] = '0' + i;
      p = p - 1;
    }
  while (u != 0);

  if (sign_p && p[1] != '0')
    {
      p[0] = '-';
      p = p - 1;
    }

  return p + 1;
}

char *
itoa_local (int x)
{
  return ntoab_local (x, 10, 1);
}

char* __itoa_buf_global = "012345678911234567890";

char *
ntoab_global (long x, unsigned base, int signed_p)
{
  char *p;
  if (g_debug > 2) eputs ("ntoab 00\n");
  //char *p = __itoa_buf + 11;
  p = __itoa_buf_global + 11;
  if (g_debug > 2) eputs ("ntoab 03\n");

  p[0] = 0;
  if (g_debug > 2) eputs ("ntoab 04\n");
  p = p - 1;
  if (g_debug > 2) eputs ("ntoab 05\n");

  int sign_p = 0;
  size_t i;
  size_t u;
  size_t b = base;
  if (signed_p != 0 && x < 0)
    {
      sign_p = 1;
      /* Avoid LONG_MIN */
      u = (-(x + 1));
      u = u + 1;
    }
  else
    u = x;

  do
    {
      u = __mesabi_uldiv (u, b, &i);
      if (i > 9)
        p[0] = 'a' + i - 10;
      else
        p[0] = '0' + i;
      p = p - 1;
    }
  while (u != 0);

  if (sign_p && p[1] != '0')
    {
      p[0] = '-';
      p = p - 1;
    }

  return p + 1;
}

char *
itoa_global (int x)
{
  return ntoab_global (x, 10, 1);
}

char* __itoa_buf;

char *
ntoab (long x, unsigned base, int signed_p)
{
  if (g_debug > 2) eputs ("ntoab 00\n");
  if (__itoa_buf == 0)
    {
      if (g_debug > 2) eputs ("ntoab 01\n");
      __itoa_buf = malloc (20);
      if (g_debug > 2) eputs ("ntoab 02\n");
    }
  char *p = __itoa_buf + 11;
  if (__itoa_buf == 0)
    eputs ("MALLOC FAILED ON __itoa_buf\n");
  p = __itoa_buf + 11;
  if (g_debug > 2) eputs ("ntoab 03\n");

  p[0] = 0;
  if (g_debug > 2) eputs ("ntoab 04\n");
  p = p - 1;
  if (g_debug > 2) eputs ("ntoab 05\n");

  int sign_p = 0;
  size_t i;
  size_t u;
  size_t b = base;
  if (signed_p != 0 && x < 0)
    {
      sign_p = 1;
      /* Avoid LONG_MIN */
      u = (-(x + 1));
      u = u + 1;
    }
  else
    u = x;

  do
    {
      u = __mesabi_uldiv (u, b, &i);
      if (i > 9)
        p[0] = 'a' + i - 10;
      else
        p[0] = '0' + i;
      p = p - 1;
    }
  while (u != 0);

  if (sign_p && p[1] != '0')
    {
      p[0] = '-';
      p = p - 1;
    }

  return p + 1;
}

char *
itoa (int x)
{
  return ntoab (x, 10, 1);
}

int
main (int argc, char **argv)
{
  eputs ("local thirtythree=");
  eputs (itoa_local (33));
  eputs ("\n");
  eputs ("global thirtythree=");
  eputs (itoa_global (33));
  eputs ("\n");
  eputs ("thirtythree=");
  eputs (itoa (33));
  eputs ("\n");
}
