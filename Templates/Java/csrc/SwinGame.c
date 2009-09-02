#include "swingame_platform_NativeCore.h"
#include "swingame_platform_NativeGraphics.h"
#include "swingame_platform_NativeInput.h"

void OpenGraphicsWindow(const char *caption, jint width, jint height);
jint WindowCloseRequested();
void ProcessEvents();
void RefreshScreen();
void RefreshScreenWithFrame(jint rate);
void DrawRectangle(jint theColour, jint filled, float xPos, float yPos, jint width, jint height);
void DrawEllipse(jint theColour, jint filled, float xPos, float yPos, jint width, jint height);
void ClearScreen(jint theColor);
jint ScreenWidth();
jint ScreenHeight();

jint IsKeyPressed(jint key);
jint WasKeyTyped(jint key);
jint MouseWasClicked(jint btn);
void GetMouseXY(float *x, float *y);
void DrawPixel(jint theColor, float xPos, float yPos);
void DrawLine(jint theColor, float xPos, float yPos, float x1pos, float y1pos);

void OpenAudio();
void CloseAudio();

/*int main()
{ 
    printf("swingame Java Interface."); 

    OpenGraphicsWindow("Hello C World", 800, 600);
    
    do
    {
        ProcessEvents();
        RefreshScreen();
    } while (0 == WindowCloseRequested());
    
    return 0; 
}*/

/*
 * Class:     swingame_platform_NativeCore_n_1
 * Method:    OpenGraphicsWindow
 * Signature: (Ljava/lang/String;II)V
 */
JNIEXPORT void JNICALL Java_swingame_platform_NativeCore_n_1openGraphicsWindow(JNIEnv *env, jclass cls, jstring caption, jint width, jint height)
{
    const char *lCaption = (*env)->GetStringUTFChars(env, caption, NULL);
    OpenGraphicsWindow(lCaption, width, height);
    (*env)->ReleaseStringUTFChars(env, caption, lCaption);
}

/*
 * Class:     swingame_platform_NativeCore_n_1
 * Method:    WindowCloseRequested
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_swingame_platform_NativeCore_n_1windowCloseRequested(JNIEnv *env, jclass cls)
{
    return (WindowCloseRequested() != 0 ? JNI_TRUE : JNI_FALSE);
}

/*
 * Class:     swingame_platform_NativeCore_n_1
 * Method:    ProcessEvents
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_swingame_platform_NativeCore_n_1processEvents (JNIEnv *env, jclass cls)
{
    ProcessEvents();
}

/*
 * Class:     swingame_platform_NativeCore_n_1
 * Method:    RefreshScreen
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_swingame_platform_NativeCore_n_1refreshScreen__ (JNIEnv *env, jclass cls)
{
    RefreshScreen();
}

JNIEXPORT void JNICALL Java_swingame_platform_NativeCore_n_1refreshScreen__I (JNIEnv *env, jclass cls, jint rate)
{
	RefreshScreenWithFrame(rate);
}

/*
 * Class:     swingame_platform_NativeCore_n_1
 * Method:    screenHeight
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_swingame_platform_NativeCore_n_1screenHeight (JNIEnv *env, jclass cls)
{
	return ScreenHeight();
}

/*
 * Class:     swingame_platform_NativeCore_n_1
 * Method:    screenWidth
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_swingame_platform_NativeCore_n_1screenWidth(JNIEnv *env, jclass cls)
{
	return ScreenWidth();
}

/*
 * Class:     swingame_platform_NativeGraphics
 * Method:    drawPixel
 * Signature: (IFF)V
 */
JNIEXPORT void JNICALL Java_swingame_platform_NativeGraphics_n_1drawPixel (JNIEnv *env, jclass cls, jint color, jfloat x, jfloat y)
{
    DrawPixel(color, x, y);
}

JNIEXPORT void JNICALL Java_swingame_platform_NativeGraphics_n_1drawLine (JNIEnv *env, jclass cls, jint color, jfloat x, jfloat y, jfloat x1, jfloat y1)
{
    DrawLine(color, x, y, x1, y1);
}

/*
 * Class:     swingame_platform_NativeGraphics
 * Method:    clearScreen
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_swingame_platform_NativeGraphics_n_1clearScreen (JNIEnv *env, jclass cls, jint color)
{
    ClearScreen(color);
}

/*
 * Class:     swingame_platform_NativeGraphics
 * Method:    fillRectangle
 * Signature: (IFFII)V
 */
JNIEXPORT void JNICALL Java_swingame_platform_NativeGraphics_n_1fillRectangle (JNIEnv *env, jclass cls, jint color, jfloat x, jfloat y, jint w, jint h)
{
	DrawRectangle(color, -1, x, y, w, h);
}

JNIEXPORT void JNICALL Java_swingame_platform_NativeGraphics_n_1fillEllipse (JNIEnv *env, jclass cls, jint color, jfloat x, jfloat y, jint w, jint h)
{
	DrawEllipse(color, -1, x, y, w, h);
}


/*
 * Class:     swingame_platform_NativeInput
 * Method:    mousePosition
 * Signature: (Ljava/awt/Point;)V
 */
JNIEXPORT void JNICALL Java_swingame_platform_NativeInput_n_1mousePosition (JNIEnv *env, jclass cls, jobject pnt)
{
    float x, y;
    jdouble dx, dy;
    
    GetMouseXY(&x, &y);
    
    dx = (jdouble)x; //123.5;
    dy = (jdouble)y; //120;
    
    jclass class_Point = (*env)->GetObjectClass(env, pnt);
    jmethodID id_setLocation = (*env)->GetMethodID(env, class_Point, "setLocation", "(DD)V");
    (*env)->CallVoidMethod(env, pnt, id_setLocation, dx, dy);
}

/*
 * Class:     swingame_platform_NativeInput
 * Method:    mouseClicked
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL Java_swingame_platform_NativeInput_n_1mouseClicked (JNIEnv *env, jclass cls, jint btn)
{
    //return JNI_TRUE;
    return MouseWasClicked(btn) != 0  ? JNI_TRUE : JNI_FALSE;
}

/*
 * Class:     swingame_platform_NativeInput
 * Method:    keyTyped
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL Java_swingame_platform_NativeInput_n_1keyTyped (JNIEnv *env, jclass cls, jint key)
{
	return WasKeyTyped(key) ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT jboolean JNICALL Java_swingame_platform_NativeInput_n_1keyDown (JNIEnv *env, jclass cls, jint key)
{
    return IsKeyPressed(key) ? JNI_TRUE : JNI_FALSE;
}
