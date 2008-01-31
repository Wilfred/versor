#ifndef JOYLISP_H
#define JOYLISP_H

/* Communication between GNU Emacs and the Linux Joystick Interface.

   Copyright (C) 2007, 2008 John C. G. Sturdy

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

*/

#define NAME_LENGTH 128

#define Button_Name(_s_,_b_) ((_s_)->button_labels_good ? \
         (button_names[(_s_)->btnmap[(_b_)] - BTN_MISC]) : \
         fake_button_name((_s_), (_b_)))

#define Axis_Name(_s_,_a_) (axis_names[(_s_)->axmap[(_a_)]])

/* This should be large enough, as the only variable parts output for
   joystick events are numbers and modifiers.  The maximum modifiers
   on a maximum monstrous joystick (that would be about 80 buttons,
   all pressed at the same time) come to about 500 chars.  However,
   the integer "modifers" and "used_modifiers" aren't big enough for
   that.*/
#define OUTPUT_BUF_SIZE 1024
#define COMMAND_BUF_SIZE 1024

/* max stick displacement; dividing this by the actual displacement
   gives 1, i.e. send a simulated event every 1 tick, when the stick
   is displaced maximally */

#define STICK_MAX_DISPLACEMENT 32767.0

typedef struct axis {
  /* displacement of stick from centre, split into two parts,
     the direction and the proportion */
  int direction;		/* -1, 0, 1 */
  double proportion;		/* 0.0 .. 1.0 */
  /* proportion of ticks for which this axis will produce events, at
     maximum displacement */
  double sensitivity;		/* 0.0 .. 1.0 */
  /* base value for sensitivity -- sensitivity is reset to this when
     starting motion, thus cancelling the effect of acceleration */
  double base_sensitivity;
  /* the limit of sensitivity to which acceleration can take us */
  double max_sensitivity;
  /* multiply the sensitivity by this on each tick */
  double acceleration;
  /* how much left before producing the next event */
  double countdown;
  /* label for the event we are producing */
  char *action;
} axis;

typedef struct joystick {
  /* The file descriptor on which we read this joystick */
  int fd;

  /* The first thing inside the brackets for all the event s-exps we issue.
     The user can set this (as the second arg when calling the program);
     the default value is a Lisp symbol followed by a space, and quote
     for what follows; but you could make it the start of a symbol name,
     without the space, so that each event type calls a different Lisp
     function. */
  char *event_name;

  /* The first thing inside the brackets for all the NON-event s-exps we
     issue.  The user can set this (as the third arg when calling the
     program); the default value is "joystick", so that each event type calls a
     different Lisp function. */
  char *name;

  /* how many controls there are */
  unsigned int naxes;
  unsigned int nbuttons;

  /* whether the button labels make sense */
  int button_labels_good;

  /* countdown for declaring the end of declarations: */
  unsigned int buttons_to_init;
  unsigned int axes_to_init;

  /* name indirections */
  uint16_t btnmap[KEY_MAX - BTN_MISC + 1];
  uint8_t axmap[ABS_MAX + 1];

  /* general information */
  int version;
  char brand_name[NAME_LENGTH];

  /* which one this is */
  char *device;

#ifdef OCTANTS
  int *octant_coding_positions;
  int *octant_accumulators;
  int octants_off_centre;
  int n_defined_octants;
  int *defined_octants;
#endif

  struct axis **axes;
} joystick;

typedef struct controller {
  struct joystick **sticks;
  unsigned int n_sticks;

  /* whether we are sending timestamps */
  int timestamped;

  /* Whether we are doing the timing, or leaving that to our consumer process */
  int timing;

  /* Whether we are reporting joystick positions as octants. */
  int octants;

  /* Whether we are reporting axis up and down as separate commands.
     This has effect only if timing == 0. */
  int up_down;

  /* It might be better for this to be an array, as there are about 80
     possible buttons, which is too many for a long int at the time of
     writing; but the bits of the long int are indexed according to the
     buttons the joystick actually has, and there's room for that,
     unless monster joysticks development occurs faster than CPU
     development. */
  unsigned long buttons_down;
  /* If a button is used as a modifier, when it goes up we say "release"
     rather than "up", so each button can be used as either a modifier
     (if any other buttons are pressed before it is released) or a
     button in its own right (if no other buttons are pressed before it
     is released).  So, we need to remember which buttons have been
     actually used as modifiers. */
  unsigned long used_modifiers;

  int symbolic_modifiers;

} controller;

extern char*
fake_button_name(struct joystick *stick, int button_index);

extern void
output(char *fmt, ...);

#ifdef DIAGRAM
typedef struct feature_labels {
  struct feature_labels *next;
  unsigned long modifiers;
  char *keymap;
  char *gamepad_label;
  unsigned int n_axis_labels;
  char **axis_labels;
  unsigned int n_button_labels;
  char **button_labels;
} feature_labels;

extern void parse_labels(struct joystick *stick, char *command_parsing);

extern void set_keymap(char *new_keymap);

extern void handle_auto_raise();

extern void trigger_auto_raise_later(unsigned long buttons_down);

extern void diagram_top_of_loop(struct controller *controller);

extern void require_redraw();

extern void set_window_auto_raise(int i);

extern void draw_diagram (struct controller *controller, int all);

extern void init_diagram();

extern void close_diagram();

extern void js_geometry(char *str);

#endif

#endif
