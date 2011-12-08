// fpc test.pas -k-F../Frameworks -k-framework -kSDL -k-macosx_version_min -k10.7 -k-no_pie
program Test;
uses SDL;

var
    wnd: PSDL_Window;
begin
    wnd := SDL_CreateWindow(PChar('title:Pchar'), 0, 0, 800, 600, 0);
    wnd := SDL_CreateWindow(PChar('title:Pchar'), 0, 0, 800, 600, 0);
    
    WriteLn(HexStr(wnd));
    
    Readln();
    
end.