/* Communicate between GNU Emacs and the Linux Joystick Interface.

   Copyright (C) 2007, 2008 John C. G. Sturdy

   Based on jstest.c which is Copyright (C) 1996-1999 Vojtech Pavlik
   (Sponsored by SuSE) and released under GPL2.

   Initially written 2007-08-29.

   This file is not part of GNU Emacs.

   The Emacs Joystick Interface is free software; you can redistribute
   it and/or modify it under the terms of the GNU General Public
   License as published by the Free Software Foundation; either
   version 2, or (at your option) any later version.

   The Emacs Joystick Interface is distributed in the hope that it
   will be useful, but WITHOUT ANY WARRANTY; without even the implied
   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with the Emacs Joystick Interface; see the file COPYING.  If
   not, write to the Free Software Foundation, Inc., 51 Franklin
   Street, Fifth Floor, Boston, MA 02110-1301, USA.

   Starting the program
   ====================

   This is normally done for you by `joystick-start' in the
   accompanying Emacs-Lisp file `joystick.el'.  In case you want to
   use this in some other application, here are the details anyway.

   Usage: joylisp [--device device] [--event event-label] [--name non-event-label]

   This program converts joystick events to Lisp s-expressions, and
   sends them to stdout.

   The "device" argument is the joystick device to read, such as
   /dev/js0.

   The optional "event-label" argument is something to put at the
   start of each event s-exp instead of "jse '" (which stands for
   JoyStick Event).  This is partly in case you have several
   joystick-like devices on the same system, and also lets you give
   something without the space and quote, so that each type of event
   runs a different Lisp function.

   The optional "non-event-label" argument is something to put at the
   start of each non-event s-exp instead of "joystick-".  This is
   partly in case you have several joystick-like devices on the same
   system.  The default is such that each type of non-event
   (declarations etc) runs a different Lisp function.

   Buttons
   =======

   When you press a button (say the button Trigger), an expression of
   the form

      (jse 'Trigger-down)

   is sent to stdout, and if that button is then released without any
   other buttons having been pressed,

      (jse 'Trigger-up)

   is output.

   If a button is pressed and held while another button is pressed,
   the "down" event for the first button is sent as before (as we
   don't have the technology to predict whether another button will be
   pressed before the first is released), but the second button, say
   button TopBtn, gets some modifiers included, either as a string of
   abbreviated button names, or as an octal number embedded in the
   functor name:

      (jse 'BaBt2-TopBtn-down)
      (jse 'BaBt2-TopBtn-up)

      (jse 'm200-TopBtn-down)
      (jse 'm200-TopBtn-up)

   where 200 is the octal code for just bit 7 (the modifier button
   number) being set.  The abbreviations are made of any upper-case
   letters in the full name of the button, any lower-case letters
   preceded by an upper-case letter, and any digits.  This seems to be
   a reasonably simple way of making unique abbreviations, giving the
   collection of button names provided.

   Any number of modifiers may be used at once.  (There are
   potentially nearly 80 of them, given a suitable monstrous stick!)

   When button 7 is eventually released, because it has been used as a
   modifier, instead of an "up" event, it generates a "release" event.

      (jse 'Trigger-release)

   This way, each button can be used both in its own right (press and
   release) or as a modifier (press and hold while pressing other
   buttons).

   Also, note that if you're using "-up" codes for chording, it makes
   a difference which button you take your finger off last.  This
   makes for a potentially extremely subtle chording keyboard!

   Joystick axes
   =============

   When you move a "stick" control, there are three modes of output
   written into the code, called "timing", "up_down" and "signed".  At
   the moment, it is hardwired to use "timing", but when commands to
   this program from the calling program (e.g. Emacs) are implemented,
   it'll be switchable.

   In timing mode, each joystick event is sent as one of these:

     (jse 'X-previous)
     (jse 'X-center)
     (jse 'X-next)

   and the event is sent repeatedly (except for -center), at a rate
   determined by the displacement of that stick axis from its centered
   position.

   In up_down mode, each joystick event is sent as:

     (jse 'X-previous amount)
     (jse 'X-center)
     (jse 'X-next amount)

   according to whether the direction is the one Emacs regards as
   "previous" (i.e. up or left, depending on the axis) or as "next"
   (down or right), or "center" which is output when the joystick
   returns to the center.  The "amount" is the number received from
   the joystick event, and is the amount (unsigned) by which the stick
   is displaced from the center.  The amount for "center" is
   implicitly 0, so is not sent.

   In signed mode, the joystick events are sent as:

     (jse 'X amount)

   where the amount may be positive or negative.

   In signed and up_down modes, the events are not automatically
   repeated.

   In any case, modifier numbers are sent in octal:

     (jse 'm200-X-previous amount)
     (jse 'm200-X-center)
     (jse 'm200-X-next amount)

   and it is remembered that those buttons have been used as
   modifiers, and hence generate "release" instead of "up" when
   released.

   Controlling the interface program
   =================================

   The program accepts "shell-style" commands on stdin.  The commands
   are as follows:

     acceleration ratio

       For every axis event, increase the sensitivity by the given
       ratio.  Normal speed is resumed when the axis is next centred.

     acknowledge [arg]

       With arg == 0, turn command acknowledgment off; otherwise turn
       it on.

     hatspeed
     hatspeed value
     hatspeed channel
     hatspeed channel value

       Set the hat speed.  This is the same as `sensitivity', but only
       applies to axes that are described as hat switch axes.  (These
       are `all-or-nothing' joystick axes, so you may well want to
       make them less sensitive than the ordinary ones.)

     numeric-mods
     symbolic-mods

       Set the modifier representation to numeric or symbolic.

     maxspeed value

       Set the maximum sensitivity (limit of acceleration) to value.

     quit

       Quit the joystick-to-lisp program.

     rumble

       Make the joystick / keypad rumble.  Not implemented, as there's
       no mention of this in the joystick driver documentation.

     sensitivity
     sensitivity value
     sensitivity axis value
     sensitivity axis

       With no args, or with only an axis name, report all or one of
       the sensitivities.

       With a value, and with or without an axis name, set one or all
       of the sensitivities.

       Value should be a floating-point value between 0.0 and 1.0; it
       is the proportion of internal ticks for which full displacement
       of the joystick in this output will produce an event in
       `timing' mode.  So, for example, if you set an axis'
       sensitivity to 0.25, it will produce an event at most once
       every 4 ticks.

     shock

       Make the joystick / keypad give the user a shock.  Not
       implemented, as there's no mention of this in the joystick
       driver documentation.  Also, this is just a joke entry from a
       James Bond film, but I wouldn't be surprised if someone really
       makes one sometime.

     show-ticking [arg]

       With arg == 0, don't show every internal tick.
       Otherwise, show all the internal ticks.
       Mostly meant for debugging.

     signed

       Output the joystick displacement as a signed value.  See also
       `timing' and `updown'.

     stamped [arg]

       With arg == 0, don't send timestamps before each event.
       Otherwise, send a timestamp before each event.

     tickrate [ticks-per-second]

       With argument, set the ticks per second (for the `timing'
       output) to ticks-per-second.

       With no argument, report the current tick rate.

       If you set this to a larger value (and you'll probably want to
       turn `sensitivity' down in that case), you get finer
       discrimination of the joystick position, at the expense of
       using more CPU time.  Still, that probably won't be much.

     timing

       When a joystick axis is off-center, send a stream of events, at
       a rate determined by how far off-center it is.  This is the
       default.  See also `signed' and `updown'.  The scale of the
       individual axis is set with the `sensitivity' command, and the
       overall rate of the system is set with `tickrate'.

     updown

       Output the joystick displacement as an unsigned value, and give
       the sign by issuing different event types.  See also `signed'
       and `timing' output modes.

 */


#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <getopt.h>
#include <ctype.h>
#include <math.h>

#include <linux/input.h>
#include <linux/joystick.h>

#include "joylisp.h"

static char *short_options = "a::d:e:n:vg:";

static struct option long_options[] = {
#ifdef DIAGRAM
  {"autoraise", optional_argument, 0, 'a'},
#endif
  {"device", required_argument, 0, 'd'},
  {"geometry", required_argument, 0, 'g'},
  {"event", required_argument, 0, 'e'},
  {"name", required_argument, 0, 'n'},
  {"verbose", no_argument, 0, 'v'},
  {0, 0, 0, 0}
};

char *axis_names[ABS_MAX + 1] = {
  /* most of these come straight from jstest.c, but I finished
     numbering them, to make them all unique */
  "X", "Y", "Z",		/* 0,1,2 */
  "Rx", "Ry", "Rz",		/* 3,4,5 */
  "Throttle", "Rudder",		/* 6,7 */
  "Wheel", "Gas", "Brake",	/* 8,9,10 */
  "Ax11", "Ax12", "Ax13", "Ax14", "Ax15", /* 11,12,13,14,15 */
  "Hat0X", "Hat0Y",		/* 16,17 */
  "Hat1X", "Hat1Y",		/* 18,19 */
  "Hat2X", "Hat2Y",		/* 20,21 */
  "Hat3X", "Hat3Y",		/* 22,23 */
  "Ax24", "Ax25", "Ax26", "Ax27", /* 24,25,26,27 */
  "Ax28", "Ax29", "Ax30",	  /* 28,29,30 */
};

/* Hat joysticks give "full displacement" or "no displacement" and
   nothing inbetween.  This means they repeat very fast, unless we
   take special measures.  So, we note the range of axis numbers that
   belong to hats. */
#define HAT_MIN 16
#define HAT_MAX 23

#define IS_HAT(_s_,_a_) (HAT_MIN <= ((_s_)->axmap[(_a_)]) && ((_s_)->axmap[(_a_)]) <= HAT_MAX)

char *button_names[KEY_MAX - BTN_MISC + 1] = {
  /* most of these come straight from jstest.c, but I finished
     numbering them, to make them all unique */
  "Btn0", "Btn1", "Btn2", "Btn3", "Btn4",
  "Btn5", "Btn6", "Btn7", "Btn8", "Btn9", /* 0-9 */
  "Btn10", "Btn11", "Btn12", "Btn13", "Btn14", "Btn15",	   /* 10-15 */
  "LeftBtn", "RightBtn", "MiddleBtn", "SideBtn",	   /* 16-19 */
  "ExtraBtn",						   /* 20 */
  "ForwardBtn", "BackBtn",				   /* 21,22 */
  "TaskBtn",						   /* 23 */
  "Btn24", "Btn25", "Btn26", "Btn27",			   /* 24-27 */
  "Btn28", "Btn29", "Btn30", "Btn31",			   /* 28-31 */
  "Trigger", "ThumbBtn", "ThumbBtn2", "TopBtn", "TopBtn2", /* 32-36 */
  "PinkieBtn",						   /* 37 */
  "BaseBtn", "BaseBtn2", "BaseBtn3", "BaseBtn4",	   /* 38-41 */
  "BaseBtn5", "BaseBtn6",				   /* 42-43 */
  "BtnDead",						   /* 44 */
  "BtnA", "BtnB", "BtnC",				   /* 45-47 */
  "BtnX", "BtnY", "BtnZ",				   /* 48-50 */
  "BtnTL", "BtnTR",					   /* 51,52 */
  "BtnTL2", "BtnTR2",					   /* 53,54 */
  "BtnSelect", "BtnStart",				   /* 55,56 */
  "BtnMode",						   /* 57 */
  "BtnThumbL", "BtnThumbR",				   /* 58,59 */
  "Btn60", "Btn61", "Btn62", "Btn63",			   /* 60-63 */
  "Btn64", "Btn65", "Btn66", "Btn67",			   /* 64-67 */
  "Btn68", "Btn69", "Btn70", "Btn71",			   /* 68-71 */
  "Btn72", "Btn73", "Btn74", "Btn75", "Btn76",		   /* 71-76 */
  "WheelBtn",						   /* 77 */
  "Gear up",						   /* 78 */
};

static char output_buf[OUTPUT_BUF_SIZE];

static char command_buf[COMMAND_BUF_SIZE];

/* Whether we are still running: */
static int running = 1;

/* Used to set the base repeat rate for "all-or-nothing" joystick axes. */
static double hat_sensitivity = 0.25;
static double hat_acceleration = 1.1;

/* Whether we are acknowledging commands */
static int acknowledge = 0;

/* Whether we are showing ticks -- mostly for debugging */
static int show_ticking = 0;

/* General internal state */

/* Commands don't always come in in a single read; this is for
   accumulating them until we get EOL: */
static char *command_reading = command_buf;
static int command_length;

static int tick_secs = 0;
static int tick_usecs = 50000;

void
output(char *fmt, ...)
{
  va_list ap;
  int formatted;
  va_start(ap, fmt);

  formatted = vsnprintf(output_buf, OUTPUT_BUF_SIZE, fmt, ap);

  va_end(ap);

  write(1, output_buf, strlen(output_buf));

  output_buf[0] = '\0';
}

static unsigned int
getstamptime()
/* return milliseconds since midnight */
{
  struct timeval tv;
  if (gettimeofday(&tv, NULL) == 0)
    {
      return (tv.tv_usec / 1000) + ((tv.tv_sec % (24 * 60 * 60)) * 1000);
    } else {
    return 0;
  }
}

/* 5*80 should be enough for all possible modifiers pressed at once */
static char modifiers_buf[512];
/* abbreviations of button names */
static char modifier_names[512];

static char *btn_abbrevs[KEY_MAX - BTN_MISC + 1];

static void
set_modifiers_buffer(struct controller *controller)
/* fill in modifiers_buf according to buttons_down */
{
  if (controller->buttons_down) {
    if (controller->symbolic_modifiers) {
      unsigned int i = 0;
      unsigned int mods = controller->buttons_down;
      char *dest = modifiers_buf;
      while (mods) {
	if (mods & 1) {
	  char *src = btn_abbrevs[i];
	  while (*src) {
	    *dest++ = *src++;
	  }
	  *dest++ = '-';
	}
	i++;
	mods = mods >> 1;
      }
      *dest = '\0';
    } else {
      sprintf(modifiers_buf, "m%o-", (unsigned int)(controller->buttons_down));
    }
  } else {
    modifiers_buf[0] = '\0';
  }
}

static int
channel_index_from_button_name(struct joystick *stick, char *name)
{
  int i;
  for (i = 0; i < stick->nbuttons; i++) {
    if ((Button_Name(stick, i) != NULL) &&
	(strcmp(name, Button_Name(stick, i)) == 0)) {
      return i;
      break;
    }
  }
  return -1;
}

static int
channel_index_from_axis_name(struct joystick *stick, char *name)
{
  int i;
  for (i = 0; i < stick->naxes; i++) {
    if ((Axis_Name(stick, i) != NULL) &&
	(strcmp(name, Axis_Name(stick, i)) == 0)) {
      return i;
    }
  }
  return -1;
}

static void
process_command (struct controller *controller)
{
  struct joystick *stick = controller->sticks[0];

  char command_name[COMMAND_BUF_SIZE];
  double float_arg;
  int numeric_arg;
  int has_numeric_arg = 0;
  char name_arg[COMMAND_BUF_SIZE];
  int has_name_arg = 0;
  /* index into all the possible buttons/axes */
  int channel = -1;
  /* index into the buttons/axes that we actually have */
  int channel_index = -1;
  int channel_type = -1;
  char *command_parsing;
  char *command_end;
  int i;

  command_reading[command_length] = '\0';
  command_reading += command_length;

  command_parsing = command_buf;

  while ((command_end = strchr(command_parsing, '\n')) != 0) {

    int cmd_n_parts = sscanf(command_parsing, "%s %lf", command_name, &float_arg);

    *command_end = '\0'; /* for neat acknowledgement */

    if (cmd_n_parts == 2) {
      /* command has numeric arg only */
      numeric_arg = (int)float_arg;
      has_numeric_arg = 1;
    } else if ((cmd_n_parts = sscanf(command_parsing,
				     "%s %s %lf",
				     command_name,
				     name_arg,
				     &float_arg)) >= 2) {
      if (cmd_n_parts == 3) {
	numeric_arg = (int)float_arg;
	has_numeric_arg = 1;
      }
      /* command has channel (and perhaps numeric arg) */
      /* look for the channel number and type */

      channel_index = channel_index_from_button_name(stick, name_arg);
      if (channel_index == -1) {
	channel_index = channel_index_from_axis_name(stick, name_arg);
	channel = stick->axmap[channel_index];
	channel_type = JS_EVENT_AXIS;
	has_name_arg = 1;
      } else {
	channel = stick->btnmap[channel_index];
	channel_type = JS_EVENT_BUTTON;
	has_name_arg = 1;
      }
    } else {
      /* command name only */
    }

    if (acknowledge) {
      output("(%scommand-acknowledge \"%s\" \"%.64s\")\n",
	     stick->name,
	     stick->device,
	     command_parsing);
    }

    if (strcmp(command_name, "quit") == 0) {
      running = 0;
    } else if (strcmp(command_name, "rumble") == 0) {
      /* send a rumble command to game controllers that support it */
      /* unfortunately, the Linux Joystick Driver doesn't
	 support it (yet); the person to ask would be Vojtech
	 Pavlik <vojtech@ucw.cz> */
    } else if (strcmp(command_name, "shock") == 0) {
      /* give the user an electric shock, like in that James
	 Bond film ;-) --- does that require an opto-isolated
	 joystick to avoid damaging the computing circuitry? PS
	 only joking, I don't know of any real joysticks that do
	 this -- alias it to rumble */
    } else if (strcmp(command_name, "updown") == 0) {
      /* send different events for the two directions of each
	 axis, with an unsigned number for the displacement from
	 center */
      controller->timing = 0;
      controller->up_down = 1;
    } else if (strcmp(command_name, "signed") == 0) {
      /* send the same event for both directions of the same
	 axis, with a signed number for the displacement from
	 center */
      controller->timing = 0;
      controller->up_down = 0;
    } else if (strcmp(command_name, "timing") == 0) {
      /* send different events for the two directions of each
	 axis, without any numbers, but repeating at an interval
	 set from the displacement from center */
      controller->timing = 1;
    } else if (strcmp(command_name, "show-ticking") == 0) {
      if (has_numeric_arg) {
	show_ticking = numeric_arg;
      } else {
	show_ticking = 1;
      }
    } else if (strcmp(command_name, "symbolic-mods") == 0) {
      /* send modifiers using abbreviated names */
      controller->symbolic_modifiers = 1;
    } else if (strcmp(command_name, "numeric-mods") == 0) {
      /* send combined modifiers as octal number */
      controller->symbolic_modifiers = 0;
    }  else if (strcmp(command_name, "acceleration") == 0) {
      /* set or show the acceleration, either overall or specifically */
      if (has_name_arg) {
	if (has_numeric_arg) {
	  if ((channel_type == JS_EVENT_AXIS) &&
	      (channel_index >= 0) &&
	      (channel_index < stick->naxes)) {
	    stick->axes[channel_index]->acceleration = float_arg;
	  }
	} else {
	  output("(axis-acceleration \"%s\" %lf)\n",
		 Axis_Name(stick, channel_index),
		 stick->axes[channel_index]->acceleration);
	}
      } else {
	if (has_numeric_arg) {
	  for (i = 0; i < stick->naxes; i++) {
	    if ((i < HAT_MIN) || (i > HAT_MAX)) {
	      stick->axes[i]->acceleration = float_arg;
	    }
	  }
	} else {
	  for (i = 0; i < stick->naxes; i++) {
	    output("(axis-acceleration \"%s\" %lf)\n",
		   Axis_Name(stick, i),
		   stick->axes[i]->acceleration);
	  }
	}
      }
    } else if (strcmp(command_name, "max_sensitivities") == 0) {
      /* set or show the max_sensitivities, either overall or specifically */
      if (has_name_arg) {
	if (has_numeric_arg) {
	  if ((channel_type == JS_EVENT_AXIS) &&
	      (channel_index >= 0) &&
	      (channel_index < stick->naxes)) {
	    stick->axes[channel_index]->max_sensitivity = float_arg;
	  }
	} else {
	  output("(axis-max-speed \"%s\" %lf)\n",
		 Axis_Name(stick, channel_index),
		 stick->axes[channel_index]->max_sensitivity);
	}
      } else {
	if (has_numeric_arg) {
	  for (i = 0; i < stick->naxes; i++) {
	    if ((i < HAT_MIN) || (i > HAT_MAX)) {
	      stick->axes[i]->max_sensitivity = float_arg;
	    }
	  }
	} else {
	  for (i = 0; i < stick->naxes; i++) {
	    output("(axis-max-speed \"%s\" %lf)\n",
		   Axis_Name(stick, i),
		   stick->axes[i]->max_sensitivity);
	  }
	}
      }
#ifdef OCTANTS
    } else if (strcmp(command_name, "octants") == 0) {
      char *p = command_parsing;
      /* skip command */
      while ((*p != ' ') && (*p != '\0')) {
	p++;
      }
      if (*p == ' ') {
	char *q = name_arg;
	p++;
	while ((*p != ' ') && (*p != '\0')) {
	  *q++ = *p++;
	}
	*q++ = '\0';
	p++;
	if (strcmp(name_arg, "off") == 0) {
	  octants = 0;
	} else if (strcmp(name_arg, "on") == 0) {
	  if (stick->n_defined_octants > 0) {
	    octants = 1;
	    /* reset these just in case */
	    stick->octants_off_centre = 0;
	    for (i = 0; i < stick->naxes; i++) {
	      stick->octant_accumulators[i] = 0;
	    }
	  } else {
	    output("(error \"No octants set up\")\n");
	  }
	} else {
	  if (stick->n_defined_octants > stick->naxes) {
	    output("(error \"Trying to define too many octant channels\")\n");
	  } else {
	    int last = 0;
	    int done = 0;
	    while (! done) {
	      int channel_index = channel_index_from_axis_name(stick, name_arg);
	      if (channel_index != -1) {
		stick->defined_octants[stick->n_defined_octants] = channel_index;
		stick->octant_coding_positions[channel_index] = stick->n_defined_octants;
		stick->octant_accumulators[channel_index] = 0;
		stick->n_defined_octants++;
		octants = 1;	/* implicitly enable them */
	      } else {
		output("(error \"%s cannot be an octant channel\")\n", name_arg);
	      }
	      q = name_arg;
	      while ((*p != ' ') && (*p != '\0')) {
		*q++ = *p++;
	      }
	      if (last) {
		done = 1;
	      } else {
		if (*p == '\0') {
		  last = 1;
		}
		*q++ = '\0';
		p++;
	      }
	    }
	  }
	}
      } else {
	/* todo: output octant status */
	for (i = 0; i < stick->n_defined_octants; i++) {
	  output("(octant %d %s)\n",
		 i, Axis_Name(stick, stick->defined_octants[i]));
	}
      }
#endif
    } else if (strcmp(command_name, "stamped") == 0) {
      /* send a timestamp before each action */
      if (cmd_n_parts == 2) {
	controller->timestamped = numeric_arg;
      } else {
	controller->timestamped = 1;
      }
    } else if (strcmp(command_name, "tickrate") == 0) {
      /* set the tick rate */
      double tick_time = 1.0 / float_arg;
      if (has_numeric_arg) {
	tick_secs = (int)(floor(tick_time));
	tick_usecs = (int)(((tick_time - (double)tick_secs)) * 1000000.0);
      } else {
	output("(joystick-current-tick-rate \"%s\" %lf)\n",
	       stick->device,
	       1.0 / (((double)tick_secs) + (((double)tick_usecs) / 1000000.0)));
      }
    } else if (strcmp(command_name, "sensitivity") == 0) {
      /* set or show the sensitivity, either overall or specifically */
      if (has_name_arg) {
	if (has_numeric_arg) {
	  if ((channel_type == JS_EVENT_AXIS) &&
	      (channel_index >= 0) &&
	      (channel_index < stick->naxes)) {
	    stick->axes[channel_index]->base_sensitivity =
	      stick->axes[channel_index]->sensitivity = float_arg;
	  }
	} else {
	  output("(axis-sensitivity \"%s\" %lf)\n",
		 Axis_Name(stick, channel_index),
		 stick->axes[channel_index]->sensitivity);
	}
      } else {
	if (has_numeric_arg) {
	  for (i = 0; i < stick->naxes; i++) {
	    if ((i < HAT_MIN) || (i > HAT_MAX)) {
	      stick->axes[i]->base_sensitivity =
		stick->axes[i]->sensitivity = float_arg;
	    }
	  }
	} else {
	  for (i = 0; i < stick->naxes; i++) {
	    output("(axis-sensitivity \"%s\" %lf)\n",
		   Axis_Name(stick, i),
		   stick->axes[i]->sensitivity);
	  }
	}
      }
    } else if (strcmp(command_name, "hatspeed") == 0) {
      /* set or show the hat speed, either overall or specifically */
      if (has_name_arg) {
	if (has_numeric_arg) {
	  if ((channel_type == JS_EVENT_AXIS) &&
	      (channel_index >= 0) &&
	      (channel_index < stick->naxes) &&
	      (channel >= HAT_MIN) &&
	      (channel <= HAT_MAX))

	    stick->axes[channel_index]->base_sensitivity =
	      stick->axes[channel_index]->sensitivity = float_arg;
	} else {
	  output("(hatspeed \"%s\" %lf)\n",
		 Axis_Name(stick, channel_index),
		 stick->axes[channel_index]->sensitivity);
	}
      } else {
	if (has_numeric_arg) {
	  for (i = 0; i <= stick->naxes; i++) {
	    int j = stick->axmap[i];

	    if ((j >= HAT_MIN) && (j <= HAT_MAX)) {
	    stick->axes[i]->base_sensitivity =
	      stick->axes[i]->sensitivity = float_arg;
	    }
	  }
	} else {
	  for (i = 0; i <= stick->naxes; i++) {
	    int j = stick->axmap[i];

	    if ((j >= HAT_MIN) && (j <= HAT_MAX)) {
	      output("(hatspeed \"%s\" %lf)\n",
		     Axis_Name(stick, i),
		     stick->axes[i]->sensitivity);
	    }
	  }
	}
      }
    } else if (strcmp(command_name, "acknowledge") == 0) {
      if (cmd_n_parts == 2) {
	acknowledge = numeric_arg;
      } else {
	acknowledge = 1;
      }
    } else if (strcmp(command_name, "keymap") == 0) {
#ifdef DIAGRAM
      set_keymap(name_arg);
#endif
    }
#ifdef DIAGRAM
    else if (strcmp(command_name, "labels") == 0) {
      parse_labels(stick, command_parsing);
    }
#endif
    else {
      output("(%sbad-command \"%s\")\n", stick->event_name, command_parsing);
    }

    command_parsing = command_end+1;
  }
  command_reading = command_buf;
}

static void
do_ticking (struct controller *controller)
{
  struct joystick *stick = controller->sticks[0];

  int i;
  /* In the case of a timeout on the select, we step all the
     things that could be doing countdowns, and output events for
     any that have reached 0. */
  if (show_ticking) {
    output("(%stick)\n", stick->event_name);
  }

  for (i = 0; i < stick->naxes; i++) {
    struct axis* axis= stick->axes[i];
    if (axis->proportion != 0.0) {
      axis->countdown -= axis->proportion * axis->sensitivity;
      if (axis->countdown <= 0.0) {
	axis->countdown = 1.0;
	axis->sensitivity *= axis->acceleration;
	if (axis->sensitivity > axis->max_sensitivity) {
	  axis->sensitivity = axis->max_sensitivity;
	}

	if (controller->timestamped) {
	  unsigned int stamptime = getstamptime();
	  output("(%stimestamp %d)\n", stick->event_name, stamptime);
	}
	output("(%s%s%s%s)\n",
	       stick->event_name,
	       modifiers_buf,
	       Axis_Name(stick, i),
	       axis->action);
	controller->used_modifiers |= controller->buttons_down;
      }
    }
  }

#ifdef DIAGRAM
  handle_auto_raise();
#endif
}

char
*fake_button_name(struct joystick *stick, int button_index)
/* In some circumstances, we don't get sensible values in the button
   name indices.  In that case, we make up a name based on the
   number. */
{
  static char fake_name_buffer[512];
  sprintf(fake_name_buffer, "Button-%d", button_index);
  return fake_name_buffer;
}

static void
get_joystick_config(struct joystick *stick)
{
  int fd = stick->fd;
  int i;

  stick->version = 0x000800; /* inherited from old code, maybe remove? */
  strcpy(stick->brand_name, "Unknown");

  stick->buttons_to_init = stick->axes_to_init = 0;

  /* in case of misreading, to stop runaway init loops */
  stick->naxes = 0;
  stick->nbuttons = 0;
  stick->button_labels_good = 0;

  ioctl(fd, JSIOCGVERSION, &stick->version);
  ioctl(fd, JSIOCGAXES, &stick->naxes);
  ioctl(fd, JSIOCGBUTTONS, &stick->nbuttons);
  ioctl(fd, JSIOCGNAME(NAME_LENGTH), stick->brand_name);
  ioctl(fd, JSIOCGAXMAP, stick->axmap);
  ioctl(fd, JSIOCGBTNMAP, stick->btnmap);

  output("(%sbegin-init \"%s\")\n",
	 stick->name,
	 stick->device);
  output("(%sdeclare-version \"%s\" \"%d.%d.%d\")\n",
	 stick->name,
	 stick->device,
	 stick->version >> 16,
	 (stick->version >> 8) & 0xff,
	 stick->version & 0xff);

  if (stick->nbuttons > 0 && stick->btnmap[0] < BTN_MISC) {
    int lowest = 0xffff;
    int highest = 0;
    output("(%sdeclare-unlabelled-buttons \"%s\" %d)\n",
	   stick->name, stick->device, stick->nbuttons);
    for (i = 0; i < stick->nbuttons; i++) {
      int button = stick->btnmap[i];
      if (button > highest) {
	highest = button;
      }
      if (button < lowest) {
	lowest = button;
      }
      output("(%sunlabelled-button \"%s\" %d %d)\n",
	     stick->name, stick->device, i, button);
    }
    output("(%sunlabelled-button-range \"%s\" %d %d)\n",
	   stick->name, stick->device, lowest, highest);
  } else {
    stick->buttons_to_init = stick->nbuttons;
    stick->button_labels_good = 1;
    output("(%sdeclare-buttons \"%s\" %d)\n",
	   stick->name, stick->device, stick->nbuttons);
  }
  stick->axes_to_init = stick->naxes;
  output("(%sdeclare-axes \"%s\" %d)\n",
	 stick->name, stick->device, stick->naxes);
  output("(%sdeclare-name \"%s\" \"%s\")\n",
	 stick->name, stick->device, stick->brand_name);

  stick->axes = (struct axis**) malloc ((stick->naxes + 1) * sizeof(struct axis*));
#ifdef OCTANTS
  stick->octant_coding_positions = (int*) malloc ((stick->naxes) * sizeof(int));
  stick->octant_accumulators = (int*) malloc ((stick->naxes) * sizeof(int));
  stick->defined_octants = (int*) malloc ((stick->naxes) * sizeof(int));
  stick->octants_off_centre = 0;
#endif

  for (i = 0; i < stick->naxes; i++) {
    struct axis *axis = (struct axis*)malloc(sizeof(struct axis));
    stick->axes[i] = axis;
    axis->countdown = 0.0;
    axis->proportion = 0.0;	/* shouldn't be used if not set anyway */
    axis->direction = 0;
    if (IS_HAT(stick, i)) {
      axis->base_sensitivity =
	axis->sensitivity = hat_sensitivity;
      axis->acceleration = hat_acceleration;
    } else {
      axis->base_sensitivity =
	axis->sensitivity = 1.0;
      /* probably confusing if analog sticks accelerate; the client
	 program can always set it if they like: */
      axis->acceleration = 1.0;
    }
    /* set the max (after acceleration): */
    axis->max_sensitivity = 1.0;
    axis->action = "-idle";
#ifdef OCTANTS
    axis->octant_coding_positions = -1;
    axis->octant_accumulators = 0;
    axis->defined_octants = -1;
#endif
  }

  /* make abbreviations of button names */
  {
    char *next = modifier_names;
    for (i = 0; i < stick->nbuttons; i++) {
      char prev;
      char *button_name = Button_Name(stick, i);
      btn_abbrevs[i] = next;
      prev = *button_name;
      for (; *button_name != '\0'; button_name++) {
	if (isupper(*button_name) || isupper(prev) || isdigit(*button_name)) {
	  *next++ = *button_name;
	}
	prev = *button_name;
      }
      *next++ = '\0';
    }
  }
}

static void
js_do_button_event(struct controller *controller,
		   int which_stick,
		   struct js_event *event)
{
  struct joystick *stick = controller->sticks[which_stick];

  if (event->value) {

    /* value != 0: button has been pressed */
    output("(%s%s%s-down)\n",
	   stick->event_name,
	   modifiers_buf,
	   Button_Name(stick, event->number));

    /* all the current modifiers have now been used */
    controller->used_modifiers |= controller->buttons_down;

    /* add to buttons_down after output, so it doesn't modify itself */
    controller->buttons_down |= (1 << event->number);
    set_modifiers_buffer(controller);

    /* add the button to the chord we are collecting */
    controller->chord |= (1 << event->number);
  } else {

    /* value == 0: button has been released */

    char *action = ((1 << event->number) & controller->used_modifiers) ? "release" : "up";

    /* mark all the modifiers that have been used */
    controller->used_modifiers |= controller->buttons_down;

    /* take it out of used_modifiers, as it's no longer an active modifier */
    controller->used_modifiers &= ~(1 << event->number);

    /* remove from buttons_down before output, so it doesn't modify itself */
    controller->buttons_down &= ~(1 << event->number);
    set_modifiers_buffer(controller);

    output("(%s%s%s-%s)\n",
	   stick->event_name,
	   modifiers_buf,
	   Button_Name(stick, event->number),
	   action);

    if (controller->buttons_down == 0) {
      output("(%schord %d)\n",
	     stick->name,
	     controller->chord);
      controller->chord = 0;
    }
  }

#ifdef DIAGRAM
  trigger_auto_raise_later(controller->buttons_down);
#endif

}

#ifdef OCTANTS
static int octant_decoder[3][3] = { { 7, 6, 5 },
				      { 0, 0, 4 },
				      { 1, 2, 3}};
#endif

static void
js_do_axis_event(struct controller *controller,
		 int which_stick,
		 struct js_event *event)
{
  char *action;
  int has_value = 1;
  int value = event->value;
  unsigned int which_axis = event->number;
  struct joystick *stick = controller->sticks[which_stick];

  if (controller->up_down && !controller->octants) {
    if (value == 0) {
      stick->axes[which_axis]->direction = 0;
      action = "-center";
      has_value = 0;
    } else if (value < 0) {
      stick->axes[which_axis]->direction = -1;
      action = "-previous";
      value = -value;
    } else {
      stick->axes[which_axis]->direction = 1;
      action = "-next";
    }
  } else {
    action = "";
  }

#ifdef OCTANTS
  if (octants && (stick->octant_coding_positions[which_axis] != -1)) {
    if (value == 0) {
      if (stick->octant_accumulators[which_axis] != 0) {
	stick->octants_off_centre--;
	if (stick->octants_off_centre == 0) {
	  int i;
	  int result = 0;
	  /* all octants have now been released */
	  for (i = 0; i < stick->n_defined_octants; i += 2) {
	    int x_value = stick->octant_accumulators[stick->defined_octants[i]];
	    int y_value = stick->octant_accumulators[stick->defined_octants[i+1]];
	    result = octant_decoder[((x_value < 0) ? 0 :
				     (x_value == 0) ? 1 : 2)]
	      [((y_value < 0) ? 0 :
		(y_value == 0) ? 1 : 2)];
	  }
	  output("(octants %d)\n", result);
	  for (i = 0; i < stick->naxes; i++) {
	    stick->octant_accumulators[i] = 0;
	  }
	}
      } else {
      }
    } else {
      /* value != 0 */
      if (stick->octant_accumulators[which_axis] == 0) {
	stick->octants_off_centre++;
      }
      stick->octant_accumulators[which_axis] = value;
    }
  } else
#endif
    if (controller->timing) {
      if (value < 0) {
	value = -value;
      }
      stick->axes[which_axis]->proportion = (((double)value)
					/ STICK_MAX_DISPLACEMENT);
      /* If using timing, don't issue an event immediately, but
	 wait for the countdown mechanism to do it, unless it is
	 starting from centered or has just gone back to
	 centered.  Otherwise, we get an extra event every time
	 the value changes, which can be very often, and makes
	 the joystick speed up whenever you move it. */
      if ((value == 0.0)
	  || (stick->axes[which_axis]->countdown == 0.0)
	  ) {
	output("(%s%s%s%s)\n",
	       stick->event_name,
	       modifiers_buf,
	       Axis_Name(stick, which_axis),
	       action);
      }
      if (value == 0) {
	stick->axes[which_axis]->countdown = 0.0;
	/* reset from any acceleration it has done */
	stick->axes[which_axis]->sensitivity = stick->axes[which_axis]->base_sensitivity;
      } else {
	/* give it something to count down from: */
	stick->axes[which_axis]->countdown = 1.0;
      }
      stick->axes[which_axis]->action = action;
    } else {
      if (has_value) {
	output("(%s%s%s%s %d)\n",
	       stick->event_name,
	       modifiers_buf,
	       Axis_Name(stick, which_axis),
	       action,
	       value);
      } else {
	output("(%s%s%s%s)\n",
	       stick->event_name,
	       modifiers_buf,
	       Axis_Name(stick, which_axis),
	       action);
      }
    }
  controller->used_modifiers |= controller->buttons_down;
}

/* Allocate and return a new joystick structure. */
struct joystick
*new_joystick(char *device, char *event_name, char *name)
{
  struct joystick *stick = (struct joystick*)malloc(sizeof(struct joystick));

  stick->device = device;
  stick->event_name = event_name;
  stick->name = name;

  return stick;
}

int
main (int argc, char **argv)
{
  int which_stick;
  int fd_max = 0;

  /* We allow more than one input device in each joystick process, as
     well as allowing multiple joystick processes in one Emacs.  This
     allows joysticks to modify each other's buttons.  (At that stage,
     this might well not be real joysticks, but some other kind of
     HID.  The idea would be to hack the hardware to make a multiple
     joystick device; for example, putting more buttons on the
     underside of a gamepad.) */
  struct controller the_controller;

  struct js_event js;
  struct timeval tv;
  fd_set set;

  char *device_name = "/dev/js0";
  char *event_name =  "jse '";
  char *name = "joystick-";

  int stick_number = 0;

  the_controller.timestamped = 0;
  the_controller.timing = 1;
  the_controller.octants = 0;
  the_controller.up_down = 1;
  the_controller.buttons_down = 0;
  the_controller.used_modifiers = 0;
  the_controller.symbolic_modifiers = 1;
  the_controller.chord = 0;
  the_controller.sticks = (struct joystick**)malloc(16*sizeof(struct joystick**));

  set_modifiers_buffer(&the_controller);

  /* set default options */

  while (1) {
    int option_index = 0;
    char opt = getopt_long(argc, argv,
			   short_options, long_options,
			   &option_index);

    if (opt == -1) {
      break;
    }
    
    switch (opt) {
#ifdef DIAGRAM
    case 'a':
      if ((optarg != NULL) &&
	  (isdigit(optarg[0]))) {
	set_window_auto_raise(atoi(optarg));
      } else {
	set_window_auto_raise(20);
      }
      break;
#endif
    case 'd':			/* device */
      device_name = optarg;
      the_controller.n_sticks = stick_number + 1;
      the_controller.sticks[stick_number] = new_joystick(device_name, event_name, name);
      stick_number++;
      break;
    case 'e':			/* event name */
      event_name = optarg;
      if (strlen(event_name) > 512) {
	/* avoid buffer overruns from over-long label names; truncate
	   quietly, they deserve nothing better ;-) */
	event_name[511] = '\0';
      }      break;
#ifdef DIAGRAM
    case 'g':
      js_geometry(optarg);
      break;
#endif
    case 'n':			/* name */
      name = optarg;
      if (strlen(name) > 512) {
	/* avoid buffer overruns from over-long label names; truncate
	   quietly, they deserve nothing better ;-) */
	name[511] = '\0';
      }
      break;
    case 'v':			/* verbose */
      acknowledge = 1;
      break;
    }
  }

  while ((optind < argc) && (argv[optind][0] != '\0')) {
    device_name =  argv[optind];
    fprintf(stderr, "extra stick %s\n", device_name);
    the_controller.n_sticks = stick_number + 1;
    the_controller.sticks[stick_number] = new_joystick(device_name, event_name, name);
    stick_number++;
    optind++;
  }

  if (stick_number == 0) {
    the_controller.n_sticks = stick_number + 1;
    the_controller.sticks[stick_number] = new_joystick(device_name, event_name, name);
    stick_number++;
  }

  for (which_stick = 0; which_stick < the_controller.n_sticks; which_stick++) {
    int new_fd = open(the_controller.sticks[which_stick]->device, O_RDONLY);
    if (new_fd < 0) {
      output("(device-missing \"%s\")\n", the_controller.sticks[which_stick]->device);
      perror("joylisp");
      return 1;
    }
    the_controller.sticks[which_stick]->fd = new_fd;
    if (new_fd > fd_max) {
      fd_max = new_fd;
    }
    get_joystick_config(the_controller.sticks[which_stick]);
  }

#ifdef DIAGRAM
  init_diagram();
  draw_diagram(&the_controller, 1);
#endif

  /* Make stdin non-blocking */
  {
    int flags = fcntl(0, F_GETFL);
    fcntl(0, F_SETFL, flags | O_NONBLOCK /* | O_DIRECT */);
  }

  while (running) {

    FD_ZERO(&set);
    for (which_stick = 0; which_stick < the_controller.n_sticks; which_stick++) {
      FD_SET(the_controller.sticks[which_stick]->fd, &set);
    }
    FD_SET(0, &set);

    int selected;

    tv.tv_sec = tick_secs;
    tv.tv_usec = tick_usecs;

#ifdef DIAGRAM
    diagram_top_of_loop(&the_controller);
#endif

    /* todo: allow multiple joysticks in one process, so people can
       construct monstrous multiple-joysticks (e.g. a gamepad with
       parts of another gamepad on its underside to provide extra
       buttons) */
    selected = select(fd_max+1, &set, NULL, NULL, &tv);

    switch (selected) {
    case -1:
      output("(select-error)\n");
      perror("joylisp");
      exit(1);
      break;		/* for lintage */

    case 0:
      /* In the case of a timeout on the select, we step all the
	 things that could be doing countdowns, and output events for
	 any that have reached 0. */
      do_ticking(&the_controller);
      break;

    default:
      if (FD_ISSET(0, &set)) {
	if ((command_length = read(0, command_reading, COMMAND_BUF_SIZE)) > 0) {
	  process_command(&the_controller);
	  continue;
	}
      }

      for (which_stick = 0; which_stick < the_controller.n_sticks; which_stick++) {
	if (FD_ISSET(the_controller.sticks[which_stick]->fd, &set)) {
	  if (read(the_controller.sticks[which_stick]->fd, &js, sizeof(struct js_event)) != sizeof(struct js_event)) {
#if 0
	    output("(%sread-error)\n", the_controller.sticks[which_stick]->event_name);
#endif
	    continue;
	  }

	  if (the_controller.timestamped) {
	    unsigned int stamptime = getstamptime();
	    output("(%stimestamp %d)\n", the_controller.sticks[which_stick]->event_name, stamptime);
	  }

	  switch (js.type) {

	  case JS_EVENT_BUTTON:
#ifdef DIAGRAM
	    require_redraw();
#endif
	    js_do_button_event(&the_controller, which_stick, &js);
	    break;

	  case JS_EVENT_AXIS:
#ifdef DIAGRAM
	    require_redraw();
#endif
	    js_do_axis_event(&the_controller, which_stick, &js);
	    break;

	  case JS_EVENT_INIT | JS_EVENT_BUTTON:
	    output("(%sdeclare-button \"%s\" %d '%s \"%s\")\n",
		   the_controller.sticks[which_stick]->name,
		   the_controller.sticks[which_stick]->device,
		   js.number,
		   Button_Name(the_controller.sticks[which_stick], js.number),
		   btn_abbrevs[js.number]);

	    the_controller.sticks[which_stick]->buttons_to_init--;

	    if ((the_controller.sticks[which_stick]->buttons_to_init == which_stick) &&
		(the_controller.sticks[which_stick]->axes_to_init == which_stick)) {
	      output("(%sinit-done \"%s\")\n",
		     the_controller.sticks[which_stick]->name,
		     the_controller.sticks[which_stick]->device);
	      the_controller.sticks[which_stick]->buttons_to_init =
		the_controller.sticks[which_stick]->axes_to_init = -1;
	    }
	    break;

	  case JS_EVENT_INIT | JS_EVENT_AXIS:
	    output("(%sdeclare-axis \"%s\" %d '%s)\n",
		   the_controller.sticks[which_stick]->name,
		   the_controller.sticks[which_stick]->device,
		   js.number,
		   Axis_Name(the_controller.sticks[which_stick], js.number));

	    the_controller.sticks[which_stick]->axes_to_init--;

	    if ((the_controller.sticks[which_stick]->buttons_to_init == which_stick) &&
		(the_controller.sticks[which_stick]->axes_to_init == which_stick)) {
	      output("(%sinit-done \"%s\")\n",
		     the_controller.sticks[which_stick]->name,
		     the_controller.sticks[which_stick]->device);
	      the_controller.sticks[which_stick]->buttons_to_init =
		the_controller.sticks[which_stick]->axes_to_init = -1;
	    }
	    break;

	  default:
	    if (the_controller.buttons_down) {
	      output("(%sevent-%d%s %d %d)\n",
		     the_controller.sticks[which_stick]->event_name,
		     js.type,
		     js.number,
		     modifiers_buf,
		     js.value);
	      the_controller.used_modifiers |= the_controller.buttons_down;
	    } else {
	      output("(%sevent-%d %d %d)\n",
		     the_controller.sticks[which_stick]->event_name,
		     js.type,
		     js.number,
		     js.value);
	    }
	    break;
	  }
	}
      }
    }
      
  }

#ifdef DIAGRAM
  close_diagram();
#endif

  exit(0);
}
