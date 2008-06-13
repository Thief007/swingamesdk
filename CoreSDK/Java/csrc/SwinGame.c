#include "swingame_Core.h"
#include "swingame_Audio.h"
#include "swingame_Graphics.h"
#include "swingame_Input.h"

void OpenGraphicsWindow(const char *caption, jint width, jint height);
jint WindowCloseRequested();
void ProcessEvents();
void RefreshScreen();

void DrawRectangle(jint theColour, jint filled, float xPos, float yPos, jint width, jint height);
void ClearScreen(jint theColor);

jint IsKeyPressed(jint key);
jint MouseWasClicked(jint btn);
void GetMouseXY(float *x, float *y);
void DrawPixel(jint theColor, float xPos, float yPos);

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
 * Class:     swingame_Core
 * Method:    OpenGraphicsWindow
 * Signature: (Ljava/lang/String;II)V
 */
JNIEXPORT void JNICALL Java_swingame_Core_openGraphicsWindow(JNIEnv *env, jclass cls, jstring caption, jint width, jint height)
{
    const char *lCaption = (*env)->GetStringUTFChars(env, caption, NULL);
    OpenGraphicsWindow(lCaption, width, height);
    (*env)->ReleaseStringUTFChars(env, caption, lCaption);
}

/*
 * Class:     swingame_Core
 * Method:    WindowCloseRequested
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_swingame_Core_windowCloseRequested
  (JNIEnv *env, jclass cls)
{
    return (WindowCloseRequested() != 0 ? JNI_TRUE : JNI_FALSE);
}

/*
 * Class:     swingame_Core
 * Method:    ProcessEvents
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_swingame_Core_processEvents
  (JNIEnv *env, jclass cls)
{
    ProcessEvents();
}

/*
 * Class:     swingame_Core
 * Method:    RefreshScreen
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_swingame_Core_refreshScreen
  (JNIEnv *env, jclass cls)
{
    RefreshScreen();
}

/*
 * Class:     swingame_Audio
 * Method:    openAudio
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_swingame_Audio_openAudio (JNIEnv *env, jclass cls)
{
    OpenAudio();
}

/*
 * Class:     swingame_Audio
 * Method:    closeAudio
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_swingame_Audio_closeAudio (JNIEnv *env, jclass cls)
{
    CloseAudio();
}

/*
 * Class:     swingame_Graphics
 * Method:    drawPixel
 * Signature: (IFF)V
 */
JNIEXPORT void JNICALL Java_swingame_Graphics_drawPixel (JNIEnv *env, jclass cls, jint color, jfloat x, jfloat y)
{
    DrawPixel(color, x, y);
}

/*
 * Class:     swingame_Graphics
 * Method:    clearScreen
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_swingame_Graphics_clearScreen (JNIEnv *env, jclass cls, jint color)
{
    ClearScreen(color);
}

/*
 * Class:     swingame_Input
 * Method:    getMousePosition
 * Signature: (Ljava/awt/Point;)V
 */
JNIEXPORT void JNICALL Java_swingame_Input_getMousePosition (JNIEnv *env, jclass cls, jobject pnt)
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
 * Class:     swingame_Input
 * Method:    mouseWasClicked
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL Java_swingame_Input_mouseWasClicked (JNIEnv *env, jclass cls, jint btn)
{
    //return JNI_TRUE;
    return MouseWasClicked(btn) ? JNI_TRUE : JNI_FALSE;
}