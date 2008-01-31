/* Animate communication between GNU Emacs and the Linux Joystick Interface.

   Copyright (C) 2007 John C. G. Sturdy

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
#include "gamepad_diagram.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "gamepad_diagram.h"

static XClassHint class_hint = {
  "Joystick", "Interface"
};

static unsigned long theBlackPixel;
static unsigned long theDarkGreyPixel;
static unsigned long theLightGreyPixel;
static unsigned long theWhitePixel;

static XColor rgb_DG;
static XColor hw_DG;
static XColor rgb_LG;
static XColor hw_LG;

static char *button_colors[] = {"blue", "purple", "seagreen", "red", "orange"};

#define N_COLOURS 5

#define BLUE   0
#define PURPLE 1
#define GREEN  2
#define RED    3
#define ORANGE 4

static unsigned long theColorPixels[20];

static Colormap theColormap;
static XColor rgb_colors[20];
static XColor hardware_colors[20];

typedef struct button_rect {
  short shape, x, y, w, h;
  char *label;
  int colour;
} button_rect;

#define UnknownButton(_s_)       { 0, 1, 1, 1, 1, (_s_), 0}
#define RoundButton(_s_,_x_,_y_,_c_) { 1, (_x_)-ROUND_BUTTON_SIZE/2, (_y_)-ROUND_BUTTON_SIZE/2, ROUND_BUTTON_SIZE, ROUND_BUTTON_SIZE, (_s_), (_c_)}
#define LongButton(_s_,_x_,_y_,_c_)  { 0, (_x_)-LONG_BUTTON_WIDTH/2, (_y_)-LONG_BUTTON_HEIGHT/2, LONG_BUTTON_WIDTH, LONG_BUTTON_HEIGHT, (_s_), (_c_)}
#define SmallButton(_s_,_x_,_y_,_c_) { 0, (_x_)-SMALL_BUTTON_WIDTH/2, (_y_)-SMALL_BUTTON_HEIGHT/2, SMALL_BUTTON_WIDTH, SMALL_BUTTON_HEIGHT, (_s_), (_c_)}
#define AxisLabel(_s_,_x_,_y_,_c_)   { 0, (_x_)-AXIS_LABEL_WIDTH/2, (_y_)-AXIS_LABEL_HEIGHT/2, AXIS_LABEL_WIDTH, AXIS_LABEL_HEIGHT, (_s_), (_c_)}

static button_rect axis_rects[] = {
  AxisLabel("X+", LEFT(ANALOG_CENTRE_X) + AXIS_LABEL_OFFSET, ANALOG_CENTRE_Y, RED),
  AxisLabel("X-", LEFT(ANALOG_CENTRE_X) - AXIS_LABEL_OFFSET, ANALOG_CENTRE_Y, RED),
  AxisLabel("Y+", LEFT(ANALOG_CENTRE_X), ANALOG_CENTRE_Y + AXIS_LABEL_OFFSET, RED),
  AxisLabel("Y-", LEFT(ANALOG_CENTRE_X), ANALOG_CENTRE_Y - AXIS_LABEL_OFFSET, RED),
  AxisLabel("Z+", RIGHT(ANALOG_CENTRE_X) + AXIS_LABEL_OFFSET, ANALOG_CENTRE_Y, GREEN),
  AxisLabel("Z-", RIGHT(ANALOG_CENTRE_X) - AXIS_LABEL_OFFSET, ANALOG_CENTRE_Y, GREEN),
  AxisLabel("Rz+", RIGHT(ANALOG_CENTRE_X), ANALOG_CENTRE_Y + AXIS_LABEL_OFFSET, GREEN),
  AxisLabel("Rz-", RIGHT(ANALOG_CENTRE_X), ANALOG_CENTRE_Y - AXIS_LABEL_OFFSET, GREEN),
  AxisLabel("HatOX+", HAT_CENTRE_X + AXIS_LABEL_OFFSET, HAT_CENTRE_Y, RED),
  AxisLabel("HatOX-", HAT_CENTRE_X - AXIS_LABEL_OFFSET, HAT_CENTRE_Y, RED),
  AxisLabel("HatOY+", HAT_CENTRE_X, HAT_CENTRE_Y + AXIS_LABEL_OFFSET, RED),
  AxisLabel("HatOY-", HAT_CENTRE_X, HAT_CENTRE_Y - AXIS_LABEL_OFFSET, RED)
};

#define N_AXIS_LABELS 12

/* note: some of the names are abbreviated, to look better on the diagram */
static button_rect button_rects[] = {
  /* 0-9 */
  UnknownButton("Btn0"),
  UnknownButton("Btn1"),
  UnknownButton("Btn2"),
  UnknownButton("Btn3"),
  UnknownButton("Btn4"),
  UnknownButton("Btn5"),
  UnknownButton("Btn6"),
  UnknownButton("Btn7"),
  UnknownButton("Btn8"),
  UnknownButton("Btn9"),
  /* 10-15 */
  UnknownButton("Btn10"),
  UnknownButton("Btn11"),
  UnknownButton("Btn12"),
  UnknownButton("Btn13"),
  UnknownButton("Btn14"),
  UnknownButton("Btn15"),

  /* 16-19 */
  UnknownButton("LeftBtn"),
  UnknownButton("RightBtn"),
  UnknownButton("MiddleBtn"),
  UnknownButton("SideBtn"),

  /* 20 */
  UnknownButton("ExtraBtn"),

  /* 21,
     22 */
  UnknownButton("ForwardBtn"),
  UnknownButton("BackBtn"),

  /* 23 */
  UnknownButton("TaskBtn"),

  /* 24-27 */
  UnknownButton("Btn24"),
  UnknownButton("Btn25"),
  UnknownButton("Btn26"),
  UnknownButton("Btn27"),

  /* 28-31 */
  UnknownButton("Btn28"),
  UnknownButton("Btn29"),
  UnknownButton("Btn30"),
  UnknownButton("Btn31"),

  /* 32-36 */
  RoundButton("Trig", RIGHT(FACE_X)-2, FACE_Y, BLUE),
  RoundButton("Thumb", RIGHT(FACE_X), FACE_Y-2, PURPLE),
  RoundButton("Thumb2", RIGHT(FACE_X), FACE_Y+2, GREEN),
  RoundButton("Top", RIGHT(FACE_X)+2, FACE_Y, RED),
  LongButton("TopBtn2", LEFT(SHOULDER_BUTTON_X), SHOULDER_BUTTON_Y+1+(2*LONG_BUTTON_HEIGHT), BLUE),

  /* 37 */
  LongButton("PinkieBtn", LEFT(SHOULDER_BUTTON_X), SHOULDER_BUTTON_Y+LONG_BUTTON_HEIGHT, PURPLE),

  /* 38-41 */
  LongButton("BaseBtn", RIGHT(SHOULDER_BUTTON_X), SHOULDER_BUTTON_Y+1+(2*LONG_BUTTON_HEIGHT), GREEN),
  LongButton("BaseBtn2", RIGHT(SHOULDER_BUTTON_X), SHOULDER_BUTTON_Y+LONG_BUTTON_HEIGHT, RED),
  SmallButton("Base3", LEFT(SMALL_BUTTON_X), SMALL_BUTTON_Y, ORANGE),
  SmallButton("Base4", RIGHT(SMALL_BUTTON_X), SMALL_BUTTON_Y, PURPLE),

  /* 42-43 */
  RoundButton("Base5", LEFT(ANALOG_CENTRE_X), ANALOG_CENTRE_Y, RED),
  RoundButton("Base6", RIGHT(ANALOG_CENTRE_X), ANALOG_CENTRE_Y, GREEN),

  /* 44 */
  UnknownButton("BtnDead"),

  /* 45-47 */
  UnknownButton("BtnA"),
  UnknownButton("BtnB"),
  UnknownButton("BtnC"),

  /* 48-50 */
  UnknownButton("BtnX"),
  UnknownButton("BtnY"),
  UnknownButton("BtnZ"),

  /* 51,
     52 */
  UnknownButton("BtnTL"),
  UnknownButton("BtnTR"),

  /* 53,
     54 */
  UnknownButton("BtnTL2"),
  UnknownButton("BtnTR2"),

  /* 55,
     56 */
  UnknownButton("BtnSelect"),
  UnknownButton("BtnStart"),

  /* 57 */
  UnknownButton("BtnMode"),

  /* 58,
     59 */
  UnknownButton("BtnThumbL"),
  UnknownButton("BtnThumbR"),

  /* 60-63 */
  UnknownButton("Btn60"),
  UnknownButton("Btn61"),
  UnknownButton("Btn62"),
  UnknownButton("Btn63"),

  /* 64-67 */
  UnknownButton("Btn64"),
  UnknownButton("Btn65"),
  UnknownButton("Btn66"),
  UnknownButton("Btn67"),

  /* 68-71 */
  UnknownButton("Btn68"),
  UnknownButton("Btn69"),
  UnknownButton("Btn70"),
  UnknownButton("Btn71"),

  /* 71-76 */
  UnknownButton("Btn72"),
  UnknownButton("Btn73"),
  UnknownButton("Btn74"),
  UnknownButton("Btn75"),
  UnknownButton("Btn76"),

  /* 77 */
  UnknownButton("WheelBtn"),

  /* 78 */
  UnknownButton("Gear up"),
};

static feature_labels* feature_label_list = NULL;

static int anything_happened = 1;	/* it was created, for a start */

Display *theDisplay;
int theScreen;
int theDepth;
XSetWindowAttributes theWindowAttributes;
unsigned long theWindowMask;
int border_width = 3;
Window theWindow;
XGCValues theGCVAlues;
GC theGC;
unsigned long theValueMask;
unsigned int window_auto_raise = 0;
unsigned int auto_raise_pending = 0;
unsigned int auto_lower_pending = 0;
unsigned int auto_raise_countdown = 0;
unsigned int window_x_scale = 16;
unsigned int window_y_scale = 16;
int window_x = 100;
int window_y = 100;
unsigned int window_width = 200;
unsigned int window_height = 100;
XSizeHints theSizeHints;

XPoint outlineBodyPoints[] = {
  { LEFT(MID_LOWER_CORNER_X), MID_LOWER_CORNER_Y },
  { LEFT(LOWER_CORNER_X), LOWER_CORNER_Y },
  { LEFT(HANDLE_INNER_X), HANDLE_INNER_Y },
  { LEFT(HANDLE_OUTER_X), HANDLE_OUTER_Y },
  { LEFT(HANDLE_SHOULDER_X), HANDLE_SHOULDER_Y },
  { LEFT(SHOULDER_EDGE_X), SHOULDER_EDGE_Y },
  { RIGHT(SHOULDER_EDGE_X), SHOULDER_EDGE_Y },
  { RIGHT(HANDLE_SHOULDER_X), HANDLE_SHOULDER_Y },
  { RIGHT(HANDLE_OUTER_X), HANDLE_OUTER_Y },
  { RIGHT(HANDLE_INNER_X), HANDLE_INNER_Y },
  { RIGHT(LOWER_CORNER_X), LOWER_CORNER_Y },
  { RIGHT(MID_LOWER_CORNER_X), MID_LOWER_CORNER_Y }
};
int nBodyPoints = 12;

XPoint outlineShoulderPoints[] = {
  { LEFT(SHOULDER_BOT_FACE_X), SHOULDER_BOT_FACE_Y },
  { LEFT(SHOULDER_TOP_FACE_X), SHOULDER_TOP_FACE_Y },
  { RIGHT(SHOULDER_TOP_FACE_X), SHOULDER_TOP_FACE_Y },
  { RIGHT(SHOULDER_BOT_FACE_X), SHOULDER_BOT_FACE_Y }
};
int nShoulderPoints = 4;

XFontStruct *fontStruct;
char *fontname = "8x13"; /* anything smallish should do */

void
js_geometry(char *str)
{
  XParseGeometry(str, &window_x, &window_y, &window_width, &window_height);
}

void
set_window_auto_raise(int i)
{
  window_auto_raise = i;
}

void
require_redraw()
{
  anything_happened = 1;	/* redraws the labels on next draw */
}

void
trigger_auto_raise_later(unsigned long buttons_down)
{
  if (window_auto_raise) {
    auto_raise_countdown = window_auto_raise;
    if (buttons_down == 0) {
      auto_lower_pending = 1;
    } else {
      auto_raise_pending = 1;
    }
  }
}

void
handle_auto_raise()
{
  if (window_auto_raise
      && (auto_raise_countdown > 0)
      && (--auto_raise_countdown == 0)) {
    if (auto_raise_pending) {
      XRaiseWindow(theDisplay, theWindow);
      auto_raise_pending = 0;
    } 
    if (auto_lower_pending) {
      XLowerWindow(theDisplay, theWindow);
      auto_lower_pending = 0;
    }
  }
}

void
diagram_top_of_loop(struct controller *controller)
{
  if (XPending(theDisplay)) {
    XEvent theEvent;

    XNextEvent(theDisplay, &theEvent);

    switch (theEvent.type) {
    case Expose:
      draw_diagram(controller, 1);
      break;
    default:
      break;
    }
  }

  if (anything_happened) {
    draw_diagram(controller, 0);
    anything_happened = 0;
  }
}

void
parse_labels(struct joystick *stick, char *command_parsing)
{
  char *label_block = strchr(command_parsing, ' ');
  int label_group = 0;	/* axes first, then buttons */
  int modifiers = 0;
  char *keymap = NULL;

  if ((label_block != NULL)
      && (label_block[1] == '<')) {
    char *keymap_end = strchr(label_block+1, '>');
    if (keymap_end) {
      label_block++;
      keymap = (char*)malloc(keymap_end - label_block);
      strncpy(keymap, label_block, (keymap_end - label_block) - 1);
      label_block = keymap_end + 1;
    }
  }

  if ((label_block != NULL)
      && (isdigit(label_block[1]))) {
    /* don't put this straight into the structure, we might just
       be querying */
    label_block++;
    modifiers = atol(label_block);
    label_block = strchr(label_block, ' ');
  }

  if (label_block != NULL) {
    char *first_group = strchr(label_block+1, ';');

    /* set these only if there are some labels to set */
    feature_labels *new_labels = (feature_labels*)malloc(sizeof(feature_labels));
    new_labels->gamepad_label = "Emacs Gamepad Interface";
    new_labels->axis_labels = NULL;
    new_labels->n_axis_labels = 0;
    new_labels->button_labels = NULL;
    new_labels->n_button_labels = 0;

    new_labels->next = feature_label_list;
    feature_label_list = new_labels;
    new_labels->keymap = keymap;
    new_labels->modifiers = modifiers;

    label_block++;

    require_redraw();

    if (first_group) {
      char *gamepad_label = (char*)malloc((first_group - label_block) + 1);

      strncpy(gamepad_label, label_block, first_group - label_block);
      gamepad_label[first_group - label_block] = '\0';
      new_labels->gamepad_label = gamepad_label;

      label_block = first_group+1;

      while (label_group <= 1) {
	int n, i;
	char **these_labels;
	char *c, *labels_copy;

	for (n = 1, c = label_block; (*c != '\0') && (*c != ';'); c++) {
	  if (*c == '|') {
	    n++;
	  }
	}

	these_labels = (char**)malloc(n*sizeof(char*));

	if (label_group == 0) {
	  new_labels->n_axis_labels = n;
	  new_labels->axis_labels = these_labels;
	} else {
	  new_labels->n_button_labels = n;
	  new_labels->button_labels = these_labels;
	}

	labels_copy = (char*)malloc((c - label_block)+2);
	these_labels[0] = labels_copy;
	strncpy(labels_copy, label_block, c - label_block);
	labels_copy[(c - label_block)+0] = '\0';

	if (*c == ';') {
	  /* move on to the next lot of labels */
	  label_block = c + 1;
	}
	  
	c = strchr(labels_copy, '|');
	i = 1;
	while (c) {
	  *c++ = '\0';
	  these_labels[i++] = c;
	  c = strchr(c, '|');
	}
	label_group++;
      }
    }
  } else {
    feature_labels *label;

    for (label = feature_label_list;
	 label != NULL;
	 label = label->next) {
      if (((modifiers == 0)
	   || (label->modifiers == modifiers))
	  && ((keymap == NULL)
	      || ((label->keymap != NULL)
		  && (strcmp(keymap, label->keymap) == 0)))){
	int i;
	output("(joystick-label \"%s\" %ld \"%s\" \"%s\")\n",
	       stick->device,
	       label->modifiers,
	       label->keymap ? label->keymap : "",
	       label->gamepad_label);
	for (i = 0; i < label->n_axis_labels; i++) {
	  output("(joystick-label-axis \"%s\" %d \"%s\" %d \"%s\")\n",
		 stick->device,
		 label->modifiers,
		 label->keymap ? label->keymap : "",
		 i, label->axis_labels[i]);
	}
	for (i = 0; i < label->n_button_labels; i++) {
	  output("(joystick-label-button \"%s\" %d \"%s\" %d \"%s\")\n",
		 stick->device,
		 label->modifiers,
		 label->keymap ? label->keymap : "",
		 i, label->button_labels[i]);
	}
      }
    }

    if (keymap) {
      free(keymap);
    }
  }
}

static char *keymap = NULL;

void
set_keymap(char *new_keymap)
{
  if ((new_keymap == NULL) ||
      (strlen(new_keymap) == 0)) {
    if (keymap) {
      free(keymap);
    }
    keymap = NULL;
  } else {
    keymap = (char*)malloc(strlen(new_keymap));
    strcpy(keymap, new_keymap);
  }
}

void
draw_diagram (struct controller *controller,
	      int all)
{
  struct joystick *stick = controller->sticks[0];
  int i;
  feature_labels *current_labels = feature_label_list;
  char *gamepad_label = "Emacs gamepad interface";

  unsigned long buttons_down = controller->buttons_down;

  int half_ascent = fontStruct->ascent / 2;
  int descent = fontStruct->descent;

  while ((current_labels != NULL) &&
	 (current_labels->modifiers != buttons_down) &&
	 !(((keymap == NULL) && (current_labels->keymap == NULL)) ||
	   ((keymap != NULL) && (current_labels->keymap != NULL) &&
	    (strcmp(keymap, current_labels->keymap) == 0)))) {
    current_labels = current_labels->next;
  }

  if (current_labels) {
    gamepad_label = current_labels->gamepad_label;
  }

#if 1
  /* deal with overflowing labels */
  all = 1;
#endif

  if (all) {
    XSetForeground(theDisplay, theGC, theWhitePixel);

    XDrawRectangle(theDisplay, theWindow, theGC, 0, 0, window_width, window_height);

    XSetForeground(theDisplay, theGC, theDarkGreyPixel);
    XFillPolygon(theDisplay, theWindow, theGC,
		 outlineBodyPoints, nBodyPoints,
		 Complex,
		 CoordModeOrigin);
    XFillArc(theDisplay, theWindow, theGC,
	     (LEFT(ANALOG_CENTRE_X) - (BULGE_DIAMETER / 2)) * window_x_scale,
	     (ANALOG_CENTRE_Y - (BULGE_DIAMETER / 2)) * window_y_scale,
	     BULGE_DIAMETER * window_x_scale,
	     BULGE_DIAMETER * window_y_scale,
	     180*64, 180*64);
    XFillArc(theDisplay, theWindow, theGC,
	     (RIGHT(ANALOG_CENTRE_X) - (BULGE_DIAMETER / 2)) * window_x_scale,
	     (ANALOG_CENTRE_Y - (BULGE_DIAMETER / 2)) * window_y_scale,
	     BULGE_DIAMETER * window_x_scale,
	     BULGE_DIAMETER * window_y_scale,
	     180*64, 180*64);
    XFillArc(theDisplay, theWindow, theGC,
	     LEFT(HANDLE_OUTER_X) * window_x_scale,
	     (HANDLE_OUTER_Y - HANDLE_END_DIAMETER / 2) * window_y_scale,
	     HANDLE_END_DIAMETER * window_x_scale,
	     HANDLE_END_DIAMETER * window_y_scale,
	     180*64, 180*64);
    XFillArc(theDisplay, theWindow, theGC,
	     RIGHT(HANDLE_INNER_X) * window_x_scale,
	     (HANDLE_OUTER_Y - HANDLE_END_DIAMETER / 2) * window_y_scale,
	     HANDLE_END_DIAMETER * window_x_scale,
	     HANDLE_END_DIAMETER * window_y_scale,
	     180*64, 180*64);
  }  

  XSetForeground(theDisplay, theGC, theLightGreyPixel);
  XFillPolygon(theDisplay, theWindow, theGC,
	       outlineShoulderPoints, nShoulderPoints,
	       Complex,
	       CoordModeOrigin);
  XFillArc(theDisplay, theWindow, theGC,
	   (HAT_CENTRE_X - FACE_CIRCLE_DIA / 2) * window_x_scale,
	   (HAT_CENTRE_Y - FACE_CIRCLE_DIA / 2) * window_y_scale,
	   FACE_CIRCLE_DIA * window_x_scale,
	   FACE_CIRCLE_DIA * window_y_scale,
	   0, 360 * 64);
  XFillArc(theDisplay, theWindow, theGC,
	   (TRIGGERS_CENTRE_X - FACE_CIRCLE_DIA / 2) * window_x_scale,
	   (TRIGGERS_CENTRE_Y - FACE_CIRCLE_DIA / 2) * window_y_scale,
	   FACE_CIRCLE_DIA * window_x_scale,
	   FACE_CIRCLE_DIA * window_y_scale,
	   0, 360 * 64);
  XFillRectangle(theDisplay, theWindow, theGC,
		 FACE_RECT_X * window_x_scale, FACE_RECT_Y * window_y_scale,
		 FACE_RECT_W * window_x_scale, FACE_RECT_H * window_y_scale);

  /* Label the whole thing */
  XSetForeground(theDisplay, theGC, theBlackPixel);
  XDrawString(theDisplay, theWindow, theGC,
	      ((LEFT(TITLE_X) * window_x_scale) -
	       (XTextWidth(fontStruct,
			   gamepad_label,
			   strlen(gamepad_label)) / 2)),
	      (TITLE_Y * window_y_scale),
	      gamepad_label, strlen(gamepad_label));
  XSetForeground(theDisplay, theGC, theWhitePixel);

  /* joystick bases */
  XFillArc(theDisplay, theWindow, theGC,
	   (LEFT(ANALOG_CENTRE_X) - (ANALOG_BASE_SIZE / 2)) * window_x_scale,
	   (ANALOG_CENTRE_Y - (ANALOG_BASE_SIZE / 2)) * window_y_scale,
	   ANALOG_BASE_SIZE * window_x_scale, ANALOG_BASE_SIZE * window_y_scale,
	   0, 360*64);
  XFillArc(theDisplay, theWindow, theGC,
	   (RIGHT(ANALOG_CENTRE_X) - (ANALOG_BASE_SIZE / 2)) * window_x_scale,
	   (ANALOG_CENTRE_Y - (ANALOG_BASE_SIZE / 2 )) * window_y_scale,
	   ANALOG_BASE_SIZE * window_x_scale, ANALOG_BASE_SIZE * window_y_scale,
	   0, 360*64);
  XFillArc(theDisplay, theWindow, theGC,
	   (HAT_CENTRE_X - (HAT_BASE_SIZE / 2)) * window_x_scale,
	   (HAT_CENTRE_Y - (HAT_BASE_SIZE / 2 )) * window_y_scale,
	   HAT_BASE_SIZE * window_x_scale, HAT_BASE_SIZE * window_y_scale,
	   0, 360*64);

  XSetForeground(theDisplay, theGC, theBlackPixel);

  /* joystick outlines */
  XDrawArc(theDisplay, theWindow, theGC,
	   (LEFT(ANALOG_CENTRE_X) - (ANALOG_BASE_SIZE / 2)) * window_x_scale,
	   (ANALOG_CENTRE_Y - (ANALOG_BASE_SIZE / 2)) * window_y_scale,
	   ANALOG_BASE_SIZE * window_x_scale, ANALOG_BASE_SIZE * window_y_scale,
	   0, 360*64);
  XDrawArc(theDisplay, theWindow, theGC,
	   (RIGHT(ANALOG_CENTRE_X) - (ANALOG_BASE_SIZE / 2)) * window_x_scale,
	   (ANALOG_CENTRE_Y - (ANALOG_BASE_SIZE / 2 )) * window_y_scale,
	   ANALOG_BASE_SIZE * window_x_scale, ANALOG_BASE_SIZE * window_y_scale,
	   0, 360*64);
  XDrawArc(theDisplay, theWindow, theGC,
	   (HAT_CENTRE_X - (HAT_BASE_SIZE / 2)) * window_x_scale,
	   (HAT_CENTRE_Y - (HAT_BASE_SIZE / 2 )) * window_y_scale,
	   HAT_BASE_SIZE * window_x_scale, HAT_BASE_SIZE * window_y_scale,
	   0, 360*64);

  /* joystick knobs */
  XFillArc(theDisplay, theWindow, theGC,
	   (((HAT_CENTRE_X - (HAT_KNOB_SIZE / 2)) +
	     (stick->axes[4]->proportion
	      * stick->axes[4]->direction
	      * AXIS_SCALE)))
	   * window_x_scale,
	   (((HAT_CENTRE_Y - (HAT_KNOB_SIZE / 2 )) +
	     (stick->axes[5]->proportion
	      * stick->axes[5]->direction
	      * AXIS_SCALE)))
	   * window_y_scale,
	   HAT_KNOB_SIZE * window_x_scale,
	   HAT_KNOB_SIZE * window_y_scale,
	   0, 360*64);
#if 1
  XFillArc(theDisplay, theWindow, theGC,
	   ((LEFT(ANALOG_CENTRE_X) - (ANALOG_KNOB_SIZE / 2)) +
	    (stick->axes[0]->proportion
	     * stick->axes[0]->direction
	     * AXIS_SCALE))
	   * window_x_scale,
	   ((ANALOG_CENTRE_Y - (ANALOG_KNOB_SIZE / 2)) +
	    (stick->axes[1]->proportion
	     * stick->axes[1]->direction
	     * AXIS_SCALE))
	   * window_y_scale,
	   ANALOG_KNOB_SIZE * window_x_scale,
	   ANALOG_KNOB_SIZE * window_y_scale,
	   0, 360*64);
  XFillArc(theDisplay, theWindow, theGC,
	   ((RIGHT(ANALOG_CENTRE_X) - (ANALOG_KNOB_SIZE / 2)) +
	    (stick->axes[3]->proportion
	     * stick->axes[3]->direction
	     * AXIS_SCALE))
	   * window_x_scale,
	   ((ANALOG_CENTRE_Y - (ANALOG_KNOB_SIZE / 2 )) +
	    (stick->axes[2]->proportion
	     * stick->axes[2]->direction
	     * AXIS_SCALE))
	   * window_y_scale,
	   ANALOG_KNOB_SIZE * window_x_scale,
	   ANALOG_KNOB_SIZE * window_y_scale,
	   0, 360*64);
#else
  {
    /* attempt to move the analog stick buttons around wholesale */
    button_rect *rect_left_analog = &button_rects[stick->btnmap[10] - BTN_MISC];
    button_rect *rect_right_analog = &button_rects[stick->btnmap[11] - BTN_MISC];
    rect_left_analog->x = ((LEFT(ANALOG_CENTRE_X) - (ANALOG_KNOB_SIZE / 2)) +
			   (stick->axes[0]->proportion * stick->axes[0]->direction * AXIS_SCALE));
    rect_left_analog->y = ((ANALOG_CENTRE_Y - (ANALOG_KNOB_SIZE / 2)) +
			   (stick->axes[1]->proportion * stick->axes[1]->direction * AXIS_SCALE));
    rect_right_analog->x = ((RIGHT(ANALOG_CENTRE_X) - (ANALOG_KNOB_SIZE / 2)) +
			    (stick->axes[3]->proportion * stick->axes[3]->direction * AXIS_SCALE));
    rect_right_analog->y = ((ANALOG_CENTRE_Y - (ANALOG_KNOB_SIZE / 2 )) +
			    (stick->axes[2]->proportion * stick->axes[2]->direction * AXIS_SCALE));
  }
#endif

  /* label the axes */
  if (current_labels) {
    for (i = 0;
	 (i < current_labels->n_axis_labels) && (i < N_AXIS_LABELS);
	 i++) {
      button_rect *rect = &axis_rects[i];
      char *label = rect->label; /* not using these for now */

      XSetForeground(theDisplay, theGC, theColorPixels[rect->colour]);

      label = current_labels->axis_labels[i];
#if 1
      /* handy for debugging label placement */
      XDrawRectangle(theDisplay, theWindow, theGC,
		     rect->x * window_x_scale,
		     rect->y * window_y_scale,
		     rect->w * window_x_scale,
		     rect->h * window_y_scale);
#endif
      XDrawImageString(theDisplay, theWindow, theGC,
		  ((rect->x + rect->w/2) * window_x_scale)
		  - (XTextWidth(fontStruct, label, strlen(label)) / 2),
		  ((rect->y) * window_y_scale) + half_ascent,
		  label, strlen(label));
    }
  }

  for (i = 0; i < stick->nbuttons; i++) {
    int b_type = stick->btnmap[i] - BTN_MISC;
    button_rect *rect = &button_rects[b_type];
    char *label = rect->label;

    if (current_labels &&
	(i < current_labels->n_button_labels)) {
      label = current_labels->button_labels[i];
    }
    if (buttons_down & (1 << i)) {
      XSetForeground(theDisplay, theGC, theBlackPixel);
      if (rect->shape) {
	XSetForeground(theDisplay, theGC, theColorPixels[rect->colour]);
	XFillArc(theDisplay, theWindow, theGC,
		 rect->x * window_x_scale, rect->y * window_y_scale,
		 rect->w * window_x_scale, rect->h * window_y_scale,
		 0, 360*64);
      } else {
	XSetForeground(theDisplay, theGC, theColorPixels[rect->colour]);
	XFillRectangle(theDisplay, theWindow, theGC,
		       rect->x * window_x_scale, rect->y * window_y_scale,
		       rect->w * window_x_scale, rect->h * window_y_scale);
      }
    } else {
      XSetForeground(theDisplay, theGC, theWhitePixel);
      if (rect->shape) {
	XFillArc(theDisplay, theWindow, theGC,
		 rect->x * window_x_scale, rect->y * window_y_scale,
		 rect->w * window_x_scale, rect->h * window_y_scale,
		 0, 360*64);
      } else {
	XFillRectangle(theDisplay, theWindow, theGC,
		       rect->x * window_x_scale, rect->y * window_y_scale,
		       rect->w * window_x_scale, rect->h * window_y_scale);
      }
#if 1
      XSetForeground(theDisplay, theGC, theColorPixels[i]);
#else
      XSetForeground(theDisplay, theGC, theBlackPixel);
#endif
      if (rect->shape) {
	XSetForeground(theDisplay, theGC, theColorPixels[rect->colour]);
	XDrawArc(theDisplay, theWindow, theGC,
		 rect->x * window_x_scale, rect->y * window_y_scale,
		 rect->w * window_x_scale, rect->h * window_y_scale,
		 0, 360*64);
	XDrawString(theDisplay, theWindow, theGC,
		    (((rect->x + rect->w/2) * window_x_scale) -
		     (XTextWidth(fontStruct, label, strlen(label)) / 2)),
		    (rect->y + rect->h/2) * window_y_scale + half_ascent,
		    label, strlen(label));
      } else {
	XSetForeground(theDisplay, theGC, theColorPixels[rect->colour]);
	XDrawRectangle(theDisplay, theWindow, theGC,
		       rect->x * window_x_scale, rect->y * window_y_scale,
		       rect->w * window_x_scale, rect->h * window_y_scale);
	XDrawString(theDisplay, theWindow, theGC,
		    (((rect->x + rect->w/2) * window_x_scale) -
		     (XTextWidth(fontStruct, label, strlen(label)) / 2)),
		    (rect->y + rect->h) * window_y_scale - descent,
		    label, strlen(label));
      }
    }
  }

  XFlush(theDisplay);
}

void
init_diagram()
{
  {
    int i;
    for (i = 0; i < nBodyPoints; i++) {
      outlineBodyPoints[i].x *= window_x_scale;
      outlineBodyPoints[i].y *= window_y_scale;
    }
    for (i = 0; i < nShoulderPoints; i++) {
      outlineShoulderPoints[i].x *= window_x_scale;
      outlineShoulderPoints[i].y *= window_y_scale;
    }
  }

  theDisplay = XOpenDisplay(NULL);

  if (theDisplay == NULL) {
    fprintf(stderr, "Could not open connection to X on display %s\n",
	    XDisplayName(NULL));
    exit(1);
  }

  theScreen = DefaultScreen(theDisplay);
  theDepth = DefaultDepth(theDisplay, theScreen);
  theColormap = DefaultColormap(theDisplay, theScreen);

  theWindowAttributes.border_pixel = BlackPixel(theDisplay, theScreen);
  theWindowAttributes.background_pixel = WhitePixel(theDisplay, theScreen);
  theWindowAttributes.override_redirect = False;
  theWindowMask = (CWBackPixel | CWBorderPixel | CWOverrideRedirect);

  window_width = window_x_scale * 32;
  window_height = window_y_scale * 22;

  theWindow = XCreateWindow(theDisplay,
			    RootWindow(theDisplay, theScreen),
			    window_x, window_y,
			    window_width, window_height,
			    border_width,
			    theDepth,
			    InputOutput,
			    CopyFromParent,
			    theWindowMask,
			    &theWindowAttributes);

  theSizeHints.flags = PPosition | PSize;
  theSizeHints.x = window_x;
  theSizeHints.y = window_y;
  theSizeHints.width = window_width;
  theSizeHints.height = window_height;

  XSetNormalHints(theDisplay, theWindow, &theSizeHints);

  XMapRaised(theDisplay, theWindow);

  theValueMask = 0L;
  theGC = XCreateGC(theDisplay, theWindow, theValueMask, &theGCVAlues);

  theBlackPixel = BlackPixel(theDisplay, theScreen);
  theWhitePixel = WhitePixel(theDisplay, theScreen);

  XSetForeground(theDisplay, theGC, theBlackPixel);
  XSetBackground(theDisplay, theGC, theWhitePixel);

  if (XLookupColor(theDisplay, theColormap, "LightGrey",
		   &rgb_LG,
		   &hw_LG) &&
      XAllocColor(theDisplay, theColormap, &hw_LG)) {
    theLightGreyPixel = hw_LG.pixel;
  } else {
    theLightGreyPixel = theBlackPixel;
  }

  if (XLookupColor(theDisplay, theColormap, "DimGrey",
		   &rgb_DG,
		   &hw_DG) &&
      XAllocColor(theDisplay, theColormap, &hw_DG)) {
    theDarkGreyPixel = hw_DG.pixel;
  } else {
    theDarkGreyPixel = theBlackPixel;
  }

  if (theDepth > 1)
    {
      int i_color_button;
      for (i_color_button = 0; i_color_button < N_COLOURS; i_color_button++) {
	if (XLookupColor(theDisplay, theColormap, button_colors[i_color_button],
			 &rgb_colors[i_color_button],
			 &hardware_colors[i_color_button]) &&
	    XAllocColor(theDisplay, theColormap, &hardware_colors[i_color_button])) {
	  theColorPixels[i_color_button] = hardware_colors[i_color_button].pixel;
	} else {
	  theColorPixels[i_color_button] = theBlackPixel;
	}
      }
    }

  fontStruct = XLoadQueryFont(theDisplay, fontname);
  if (fontStruct) {
    XSetFont(theDisplay, theGC, fontStruct->fid);
  }

  XSelectInput(theDisplay, theWindow, ExposureMask);

  {
    XSetClassHint(theDisplay, theWindow, &class_hint);
  }

  XFlush(theDisplay);
}

void
close_diagram()
{
  XFreeFont(theDisplay, fontStruct);
  XUnmapWindow(theDisplay, theWindow);
  XDestroyWindow(theDisplay, theWindow);
  XCloseDisplay(theDisplay);
}
