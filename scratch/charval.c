/* CHARVAL.C -- echo hexidecimal, decimal, octal, and binary values of typed
   chars (and print ASCII meaning where applicable)

   With an optional argument, will also echo the raw character.
   This is useful when your terminal is in "transparent" mode, for instance.

   Written by Allan Heydon. Hacked by Olin Shivers 3/90.
*/

#include <termios.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>

#define TERMINAL 0		/* file descriptor 0 */
#define QUIT 'q'		/* quit character */

static char *desc[] = {
  "Null (NUL)",
  "(SOH)",
  "(STX)",
  "(ETX)",
  "(EOT)",
  "(ENQ)",
  "Acknowledge (ACK)",
  "Bell (BEL)",
  "Backspace (BS)",
  "[Horizontal] Tab (HT)",
  "Linefeed (NL) [New Line]",
  "Vertical Tab (VT)",
  "Form Feed (NP) [New Page]",
  "Carriage Return (CR)",
  "(SO)",
  "(SI)",
  "(DLE)",
  "XON (DC1)",
  "(DC2)",
  "XOFF (DC3)",
  "(DC4)",
  "(NAK)",
  "(SYN)",
  "(ETB)",
  "(CAN)",
  "(EM)",
  "(SUB)",
  "Escape (ESC)",
  "(FS)",
  "(GS)",
  "(RS)",
  "(US)",
  "Space (SP)"
};

#define DEL 0x7f

static char delete_desc[] = "Delete (DEL) Rubout";
static void binprint();

/* Optional argument means echo the character raw. */
main(int ac)
{
  int val, cval; /* val is raw value. cval is cooked: meta bit cleared. */
  struct termios tty, tty0;

  ac--;
  printf("Enter characters to evaluate, %c or meta-%c to quit.\n\n",QUIT,QUIT);
  puts(ac ? "RawChar\tChar\tDecimal\tHex\tOctal\tBinary\t     [Meaning]" :
       "Char\tDecimal\tHex\tOctal\tBinary\t     [Meaning]");

  tcgetattr(TERMINAL, &tty);
  tty0 = tty;
  tty.c_iflag &= ~(IGNBRK|BRKINT|PARMRK|INLCR|IGNCR|ICRNL|IXON);
  tty.c_oflag &= ~(OPOST|ONLCR|OCRNL|ONOCR|ONLRET);
  tty.c_lflag &= ~(ECHO|ICANON|ISIG|IEXTEN);
  tcsetattr(TERMINAL, TCSADRAIN, &tty);

  do {
    val = getchar();
    cval = toascii(val);
    if( ac ) printf("%c\t", val);	/* Output the raw char itself. */

    /* Output the char cooked: c, ^c, m-c, m-^c. */
    fputs( isascii(val) ? (isprint(cval) ? "   " : "  ^")
           : (isprint(cval) ? " m-" : "m-^"), stdout );
    putchar( isprint(cval) ? cval : (cval==DEL ? '?' : cval+'@') );

    /* Output the char as a number: decimal, hex, octal, binary. */
    printf("\t%3d\t0x%02x\t0%03o\t",val,val,val);
    binprint(val);

    /* Output the name of the char: NUL, SOH, BEL, ... */
    if (!isprint(cval))
	    printf("     %s", cval == DEL ? delete_desc : desc[cval]);

    fputs("\n\r", stdout);
  } while (cval != QUIT); /* QUIT or meta-QUIT will do it. */

  /* These should probably be on a signal handler, too. WTF. */
  tcsetattr(TERMINAL, TCSADRAIN, &tty0);
}

static void binprint(c)             /* print a byte in binary */
register int c;
{
  register int i;
  for (i=8; i--;) putchar("01"[1 & (c >> i)]);
}
