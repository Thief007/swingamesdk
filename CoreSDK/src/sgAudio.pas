//=============================================================================
// sgAudio.pas
//=============================================================================
//
// The Audio unit is responsible for managing SDL audio for music and sound
// effects. This includes initialisation, loading, freeing, playing, and
// checking if music or sound is playing.
//
// Change History:
//
// Version 3:
// - 2009-07-29: Andrew : Open Audio now indicates if audio has been opened.
// - 2009-07-14: Andrew : Removed loading and freeing code.
// - 2009-06-16: Clinton: Commenting/format tweaks
// - 2009-06-04: Andrew : Finished processing comments.
//                        Added fading capabilities.
//                        Fixed comments in implementation.
//
// Version 2.1:
// - 2009-05-19: Andrew:  Added PlaySoundEffect with volume
//                        Added meta comments
//
// Version 2.0:
// - 2008-12-17: Andrew: Moved all integers to LongInt
// - 2008-12-16: Andrew: Added volume controls
//
// Version 1.1:
// - 2008-03-09: Andrew: Added extra exception handling
// - 2008-01-17: Aki + Andrew: Refactor
//
// Version 1.0:
// - Various
//=============================================================================

/// SwinGame's Audio is responsible for loading and playing music and sound
/// effects. The main functionality exists in `LoadMusic`, `PlayMusic`,
/// `LoadSoundEffect`, and `PlaySoundEffect`. Associated with these are the
/// `Music` and `SoundEffect` types.
///
///@module Audio
///@static
unit sgAudio;

//=============================================================================
interface
//=============================================================================

  uses sgTypes;
  
  /// `TryOpenAudio` attempts to open the audio device for SwinGame to use.
  /// If this fails `TryOpenAudio` returns false to indicate that the audio
  /// device has not opened correctly and audio cannot be played.
  ///
  /// @lib
  function TryOpenAudio(): Boolean;
  
  /// `OpenAudio` is used to initialise the SwinGame audio code. This should be
  /// called at the start of your programs code, and is usually coded into the
  /// starting project templates. After initialising the audio code you can
  /// load and play `Music` using `LoadMusic` and `PlayMusic', load and play
  /// `SoundEffect`s using `LoadSoundEffect` and `PlaySoundEffect`. At the end
  /// of the program you need to call `CloseAudio` to ensure that the audio
  /// code is correctly terminated.
  ///
  /// @lib
  procedure OpenAudio();
  
  /// `AudioOpen` indicates if SwinGame's audio has been opened. Sound effects
  /// and Music can only be played with the audio is open.
  ///
  /// @lib
  function AudioReady(): Boolean;
  
  /// `CloseAudio` is used to clean up the resources used by SwinGame audio. If
  /// `OpenAudio` is called, this must be called to return the resources used
  /// before the program terminates.
  ///
  /// @lib
  procedure CloseAudio();
  
  /// There are several versions of PlaySoundEffect that can be used to control
  /// the way the sound effect plays, allowing you to control its volume and 
  /// the number of times the code loops. In all cases the started sound effect
  /// is mixed with the currently playing sound effects and music.
  ///
  /// With this version of PlaySoundEffect, the started sound effect will be 
  /// played at full volume.
  ///
  /// @param effect The effect indicates which sound effect to start playing. This
  ///               effect is played once at its full volume.
  ///
  /// @lib PlaySoundEffectWithLoopAndVolume(effect,1,1.0)
  /// @uname PlaySoundEffect
  ///
  /// @class SoundEffect
  /// @method Play
  procedure PlaySoundEffect(effect: SoundEffect); overload;
    
  /// This version of PlaySoundEffect allows you to indicate the number of times
  /// the sound effect is repeated. Setting the loops parameter to -1 will cause
  /// the sound effect to be looped infinitely, setting it to a value larger than
  /// 0 plays the sound effect the number of times indicated, calling with a 
  /// value of 0 means the sound effect is not played.
  ///
  /// @param effect The effect indicates which sound effect to start playing. This
  ///               effect is played once at its full volume.
  ///
  /// @param loops Controls the number of times the sound effect is played. -1
  ///              means the sound effect is repeated infinitely.
  ///
  /// @lib PlaySoundEffectWithLoopAndVolume(effect, loops, 1.0)
  /// @uname PlaySoundEffectWithLoop
  ///
  /// @class SoundEffect
  /// @overload Play PlayWithLoops
  procedure PlaySoundEffect(effect: SoundEffect; loops: LongInt); overload;
        
  /// This version of PlaySoundEffect allows you to control the volume of the 
  /// sounds playback. The vol parameter will take a value between 0 and 1 
  /// indicating the percentage of full volume to play at.
  /// For example, 0.1 plays the sound effect at 10% of its original volume.
  ///
  /// @param effect The effect indicates which sound effect to start playing. 
  /// @param vol Indicates the percentage of the original volume to play the 
  ///            `SoundEffect` at. This must be between 0 and 1.
  ///
  /// @lib PlaySoundEffectWithLoopAndVolume(effect, 1, vol)
  /// @uname PlaySoundEffectWithVolume
  /// @version 2.1
  ///
  /// @class SoundEffect
  /// @overload Play PlayWithVolume
  procedure PlaySoundEffect(effect: SoundEffect; vol: Single); overload;
  
  /// This version of PlaySoundEffect allows you to control both the number
  /// of times the `SoundEffect` is repeated, and its playback volume.
  ///
  /// @param effect The effect indicates which sound effect to start playing. 
  /// @param loops Controls the number of times the sound effect is played.
  /// @param vol Indicates the percentage of the original volume to play the 
  ///            `SoundEffect` at. This must be between 0 and 1.
  ///
  /// @lib PlaySoundEffectWithLoopAndVolume
  /// @version 2.0
  ///
  /// @class SoundEffect
  /// @overload Play PlayWithLoopsAndVolume
  procedure PlaySoundEffect(effect: SoundEffect; loops: LongInt; vol: Single); overload;

  /// PlayMusic starts playing a `Music` resource. SwinGame only allows one 
  /// music resource to be played at a time. Starting to play a new music 
  /// resource will stop the currently playing music track. You can also stop
  /// the music by calling `StopMusic`.
  ///
  /// By default SwinGame starts playing music at its full volume. This can be 
  /// controlled by calling `SetMusicVolume`. The current volume can be checked 
  /// with `MusicVolume`.
  ///
  /// To test if a `Music` resource is currently playing you can use the 
  /// `MusicPlaying` function.
  ///
  /// This version of PlayMusic can be used to play background music that is 
  /// looped infinitely. The currently playing music is stopped and the new 
  /// music resource will start playing, and will repeat until `StopMusic` is 
  /// called, or another resource is played. 
  ///
  /// @param mus The `Music` resource to play.
  ///
  /// @lib PlayMusicWithLoops(mus, -1)
  /// @uname PlayMusic
  ///
  /// @class Music
  /// @method Play
  procedure PlayMusic(mus: Music); overload;
  
  /// This version of PlayMusic allows you to control the number of times the 
  /// `Music` resource is repeated. It starts playing the supplied `Music` 
  /// resource, repeating it the numder of times specified in the loops 
  /// parameter. Setting loops to -1 repeats the music infinitely, other values
  /// larger than 0 indicate the number of times that the music should be 
  /// played.
  ///
  /// @param mus The `Music` resource to be played.
  /// @param loops The number of times that the music should be played, -1 for 
  ///              repeat infinitely
  ///
  /// @lib PlayMusicWithLoops
  ///
  /// @class Music
  /// @overload Play PlayWithLoops
  procedure PlayMusic(mus: Music; loops: LongInt); overload;
  
  /// Fades the music in over a number of milliseconds, and then continues to
  /// play the music repeatedly until the program ends or the music is stopped. 
  /// The music fades from 0 volume up to the currently set music volume.
  /// 
  /// @param mus The `Music` resource to be played.
  /// @param ms The number of milliseconds over which to fade the music in to 
  //            the current music volume.
  ///
  /// @lib FadeMusicIn(mus, -1, ms)
  /// @uname FadeMusicIn
  ///
  /// @class Music
  /// @method FadeIn
  procedure FadeMusicIn(mus: Music; ms: LongInt); overload;

  /// This version of FadeMusicIn fades the music in then plays the 'Music' 
  /// for a given number of loops.Setting loops to -1 repeats the music 
  /// infinitely, other values larger than 0 indicate the number of times that
  /// the music should be played.
  ///
  /// @param mus The `Music` resource to be played.
  /// @param loops The number of times that the music should be played, -1 for 
  ///              repeat infinitely
  /// @param ms The number of milliseconds over which to fade the music in to 
  ///           the current music volume.
  ///
  /// @lib FadeMusicIn
  /// @uname FadeMusicInWithLoops
  ///
  /// @class Music
  /// @overload FadeIn FadeInWithLoops
  procedure FadeMusicIn(mus: Music; loops, ms: LongInt); overload;
  
  /// This procedure allows you to set the volume of the currently playing 
  /// music. The vol parameter indicates the percentage of the original volume,
  /// for example, 0.1 sets the playback volume to 10% of its full volume.
  ///
  /// @param value Indicates the percentage of the original volume to play the 
  ///            `Music` at. This must be between 0 and 1, e.g. 0.1 is 10%.
  ///
  /// @lib SetMusicVolume
  ///
  /// @class Music
  /// @static
  /// @setter Volume
  procedure SetMusicVolume(value: Single);
  
  /// This function returns the current volume of the music. This will be a 
  /// value between 0 and 1, with 1 indicating 100% of the `Music` resources
  /// volume.
  ///
  /// @returns The volume of the currently playing music.
  ///
  /// @lib MusicVolume
  ///
  /// @class Music
  /// @static
  /// @getter Volume
  function MusicVolume(): Single;
  
  /// This function indicates if music is currently playing. As only one music 
  /// resource can be playing at a time this does not need to be told which
  /// music resource to check for.
  ///
  /// @returns true if the music is playing
  ///
  /// @lib MusicPlaying
  ///
  /// @class Music
  /// @static
  /// @method IsPlaying
  function MusicPlaying(): Boolean;
  
  /// This function can be used to check if a sound effect is currently 
  /// playing. 
  ///
  /// @param effect The sound effect to check.
  /// @returns true if the effect `SoundEffect` is playing.
  ///
  /// @lib SoundEffectPlaying
  ///
  /// @class SoundEffect
  /// @method IsPlaying
  function SoundEffectPlaying(effect: SoundEffect): Boolean;

  /// Stops all occurances of the effect `SoundEffect` that is currently playing.
  ///
  /// @param effect The sound to stop.
  ///
  /// @lib StopSoundEffect
  ///
  /// @class SoundEffect
  /// @method Stop
  procedure StopSoundEffect(effect: SoundEffect);

  /// Stops playing the current music resource.
  ///
  /// @lib StopMusic
  ///
  /// @class Music
  /// @static
  /// @method Stop
  procedure StopMusic();
  
  /// Fades the currently playing music out over a number of milli seconds.
  ///
  /// @param ms The number of milliseconds over which to fade the music to 0 volume.
  ///
  /// @lib FadeMusicOut
  ///
  /// @class Music
  /// @static
  /// @method FadeOut
  procedure FadeMusicOut(ms: LongInt);
  

//=============================================================================
implementation
//=============================================================================

  uses
    SysUtils, Classes, SDL_Mixer, SDL, sgShared;

  var
    // Contains the sound channels used to determine if a sound is currently
    // playing and enables us to stop the sound, check if it is playing etc.
    soundChannels: Array[0..7] of Pointer;
  
  function TryOpenAudio(): Boolean;
  begin
    sgShared.AudioOpen :=  Mix_OpenAudio( 44100, MIX_DEFAULT_FORMAT, 2, 2048 ) >= 0;
    result := sgShared.AudioOpen;
  end;
  
  procedure OpenAudio();
  begin
    if not TryOpenAudio() then
      RaiseException('Error opening audio device: ' + string(Mix_GetError()));
  end;
  
  function AudioReady(): Boolean;
  begin
    result := sgShared.AudioOpen;
  end;
  
  procedure CloseAudio();
  begin
    AudioOpen := False;
    Mix_CloseAudio();
  end;
  
  procedure SetMusicVolume(value: Single);
  var
    newVol: LongInt;
  begin
    if (value < 0) then value := 0
    else if value > 1 then value := 1;

    //SDL music volume is 0 - 128
    newVol := Trunc(value * 128);
    Mix_VolumeMusic(newVol);
  end;
  
  function MusicVolume(): Single;
  begin
   result := Mix_VolumeMusic(-1) / 128;
  end;
  
  procedure PlaySoundEffect(effect: SoundEffect; loops: LongInt; vol: Single); overload;
  var
    i: LongInt;
  begin
    if not Assigned(effect) then
    begin
      RaiseException('Sound not supplied');
      exit;
    end;
    //dont play if loops = 0
    if loops = 0 then exit;
    
    //correct volume to be between 0 and 1
    if (vol < 0) then vol := 0
    else if vol > 1 then vol := 1;
    
    //alter repeats for multiple loops
    if loops >= 1 then loops := loops- 1;
    
    //play the effect, seaching for a channel
    i := Mix_PlayChannel( -1, effect, loops);
    if i <> -1 then
    begin
      Mix_Volume(i, Trunc(vol * 128));
      soundChannels[i] := effect;
    end;
  end;
  
  procedure PlaySoundEffect(effect: SoundEffect; loops: LongInt); overload;
  begin
    PlaySoundEffect(effect, loops, 1.0)
  end;
  
  procedure PlaySoundEffect(effect: SoundEffect); overload;
  begin
    PlaySoundEffect(effect, 1, 1.0);
  end;
  
  procedure PlaySoundEffect(effect: SoundEffect; vol: Single); overload;
  begin
   PlaySoundEffect(effect, 1, vol);
  end;
  
  procedure PlayMusic(mus: Music; loops: LongInt); overload;
  begin
    //Mix_HaltMusic();
    if not Assigned(mus) then begin RaiseException('Music not supplied'); exit; end;
    Mix_PlayMusic(mus, loops);
  end;
  
  procedure PlayMusic(mus: Music); overload;
  begin
    PlayMusic(mus, -1);
  end;

  procedure FadeMusicIn(mus: Music; loops, ms: LongInt); overload;
  begin
    if not Assigned(mus) then begin RaiseException('Music not supplied'); exit; end;
    Mix_FadeInMusic(mus, loops, ms);
  end;
  
  procedure FadeMusicIn(mus: Music; ms: LongInt); overload;
  begin
    FadeMusicIn(mus, -1, ms)
  end;
  
  procedure FadeMusicOut(ms: LongInt);
  begin
    Mix_FadeOutMusic(ms);
  end;
  
  function SoundPlaying(effect: Pointer): Boolean;
  var
    i: LongInt;
  begin
    result := false;
    
    for i := 0 to High(soundChannels) do
    begin
      if soundChannels[i] = effect then
      begin
        result := result or (Mix_Playing(i) <> 0);
        if result then exit;
      end;
    end;
  end;
  
  function SoundEffectPlaying(effect: SoundEffect): Boolean;
  begin
    result := SoundPlaying(effect);
  end;
  
  function MusicPlaying(): Boolean;
  begin
    result := Mix_PlayingMusic() <> 0;
  end;
  
  procedure StopMusic();
  begin
    Mix_HaltMusic();
  end;
  
  procedure StopSoundEffect(effect: SoundEffect);
  var
    i: LongInt;
  begin
    for i := 0 to High(soundChannels) do
    begin
      if soundChannels[i] = effect then
      begin
        Mix_HaltChannel(i);
        soundChannels[i] := nil;
      end;
    end;
  end;

//=============================================================================

  initialization
  begin
    InitialiseSwinGame();
  end;

end.
