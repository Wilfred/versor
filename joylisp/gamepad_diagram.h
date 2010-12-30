#define LEFT(_a_) (15 - (_a_))
#define RIGHT(_a_) (15 + (_a_))

#define SCALE(_b_) (_b_)

/* the boundary between the face/handle and the shoulder */
#define HANDLE_SHOULDER_X  12
#define HANDLE_SHOULDER_Y  (((LONG_BUTTON_HEIGHT + 1) * LONG_BUTTON_ROWS))

#define MID_LOWER_CORNER_X  0
#define MID_LOWER_CORNER_Y (10 + HANDLE_SHOULDER_Y)
#define LOWER_CORNER_X      8
#define LOWER_CORNER_Y     (10 + HANDLE_SHOULDER_Y)
#define HANDLE_INNER_X     10
#define HANDLE_INNER_Y     (16 + HANDLE_SHOULDER_Y)
#define HANDLE_OUTER_X     14
#define HANDLE_OUTER_Y     (16 + HANDLE_SHOULDER_Y)

/* the bulge around each analog stick */
#define BULGE_DIAMETER      (ANALOG_BASE_SIZE + ANALOG_BASE_SIZE / 2)
/* the ends of the handles */
#define HANDLE_END_DIAMETER (HANDLE_OUTER_X - HANDLE_INNER_X)

#define TITLE_X             0
#define TITLE_Y             (HANDLE_SHOULDER_Y + 2)

#define SHOULDER_EDGE_X    10
#define SHOULDER_EDGE_Y     1

#define SHOULDER_TOP_FACE_X (SHOULDER_EDGE_X - 1)
#define SHOULDER_TOP_FACE_Y (SHOULDER_EDGE_Y + 1)
#define SHOULDER_BOT_FACE_X (HANDLE_SHOULDER_X - 2)
#define SHOULDER_BOT_FACE_Y (HANDLE_SHOULDER_Y - 1)

#define SHOULDER_BUTTON_X   5
#define SHOULDER_BUTTON_Y   (SHOULDER_TOP_FACE_Y-1)

#define FACE_X              8
#define FACE_Y              (4 + HANDLE_SHOULDER_Y)

#define ANALOG_CENTRE_X     5
#define ANALOG_CENTRE_Y    (9 + HANDLE_SHOULDER_Y)
#define ANALOG_BASE_SIZE    4
#define ANALOG_KNOB_SIZE    2
#define HAT_CENTRE_X        LEFT(FACE_X)
#define HAT_CENTRE_Y        FACE_Y
#define HAT_BASE_SIZE       6
#define HAT_KNOB_SIZE       4
#define AXIS_SCALE          .7

#define TRIGGERS_CENTRE_X   RIGHT(FACE_X)
#define TRIGGERS_CENTRE_Y   FACE_Y

#define FACE_CIRCLE_DIA     8

#define FACE_RECT_X          HAT_CENTRE_X
#define FACE_RECT_Y          ((HAT_CENTRE_Y - (FACE_RECT_H / 2)) + 0)
#define FACE_RECT_W          (2 * FACE_X)
#define FACE_RECT_H          (FACE_CIRCLE_DIA - 2)

#define LONG_BUTTON_ROWS     3
#define LONG_BUTTON_WIDTH    6
#define LONG_BUTTON_HEIGHT   2

#define SMALL_BUTTON_X       2
#define SMALL_BUTTON_Y       FACE_Y

#define SMALL_BUTTON_WIDTH   3
#define SMALL_BUTTON_HEIGHT  1

#define ROUND_BUTTON_SIZE    2

#define AXIS_LABEL_WIDTH     4
#define AXIS_LABEL_HEIGHT    1
#define AXIS_LABEL_OFFSET    2
