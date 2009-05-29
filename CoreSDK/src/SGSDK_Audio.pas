// /-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
// +/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
//          SGSDK_Audio.pas
// +\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
// \-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Audio unit is responsible for managing SDL audio
// this includes initialisation, loading, freeing, 
// playing, and checking if playing.
//
// Change History:
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

/// SwinGame's Audio is responsible for loading and
/// playing music and sound effects. The main functionality
/// exists in `LoadMusic`, `PlayMusic`, `LoadSoundEffect`, and
/// `PlaySoundEffect`. Associated with these are the
/// `Music` and `SoundEffect` types.
///
///@class Audio
///@static
unit SGSDK_Audio;

interface
  uses SDL_Mixer, SDL, {blah} SGSDK_Core; //test
  
  type
    /// The SoundEffect type is used to refer to sound effects that can be played
    /// by the SwinGame audio code. Sound effects are loaded with 
    /// `LoadSoundEffect`, played using `PlaySoundEffect`, and must be
    /// released using `FreeMusic`.
    ///
    /// SwinGame will mix the audio from multiple sound effects, making it possible
    /// to play multiple SoundEffects, or even to play the one SoundEffect multiple
    /// times.
    ///
    /// You can check if a SoundEffect is currently playing using 
    /// `IsSoundEffectPlaying`. 
    /// 
    /// To stop a SoundEffect playing use `StopSoundEffect`. This will stop all
    /// instances of this one sound effect from playing.
    ///
    ///@note Use `Music` for background music for your games.
    ///
    ///@class SoundEffect
    ///@field pointer: pointer
    SoundEffect = PMix_Chunk;

    /// The SoundEffect type is used to refer to sound effects that can be played
    /// by the SwinGame audio code. Sound effects are loaded with 
    /// `LoadSoundEffect`, played using `PlaySoundEffect`, and must be
    /// released using `FreeMusic`.
    ///
    /// SwinGame will mix the audio from multiple sound effects, making it possible
    /// to play multiple SoundEffects, or even to play the one SoundEffect multiple
    /// times.
    ///
    /// You can check if a SoundEffect is currently playing using 
    /// `IsSoundEffectPlaying`. 
    /// 
    /// To stop a SoundEffect playing use `StopSoundEffect`. This will stop all
    /// instances of this one sound effect from playing.
    ///
    ///@note Use `SoundEffect` for the foreground sound effects of for your games.
    ///
    ///@class Music
    ///@field pointer: pointer
    Music = PMix_Music;

  /// OpenAudio is used to initialise the SwinGame audio code. This should be called
  /// at the start of your programs code, and is usually coded into the starting 
  /// project templates. After initialising the audio code you can load and play `Music` 
  /// using `LoadMusic` and `PlayMusic', load and play `SoundEffect`s using 
  /// `LoadSoundEffect` and `PlaySoundEffect`. At the end of the program you need to
  /// call `CloseAudio` to ensure that the audio code is correctly terminated.
  ///
  ///@lib OpenAudio
  ///@uname OpenAudio
  procedure OpenAudio();

  /// CloseAudio is used to clean up the resources used by SwinGame audio. If 
  /// `OpenAudio` is called, this must be called to return the resources used
  /// before the program terminates.
  ///
  ///@lib CloseAudio
  ///@uname CloseAudio
  procedure CloseAudio();

  /// Loads the `SoundEffect` from the supplied path. To ensure that your game
  /// is able to work across multiple platforms correctly ensure that you use
  /// `GetPathToResource` to get the full path to the files in the projects 
  /// resources folder.
  ///
  /// LoadSoundEffect can load wav and ogg audio files.
  ///
  /// `FreeSoundEffect` should be called to free the resources used by the 
  /// `SoundEffect` data once the resource is no longer needed.
  ///
  ///@param path the path to the sound effect file to load. 
  ///
  ///@lib LoadSoundEffect
  ///@uname LoadSoundEffect
  ///
  ///@class SoundEffect
  ///@constructor
  function LoadSoundEffect(path: String): SoundEffect;

  /// Loads the `Music` from the supplied path. To ensure that your game
  /// is able to work across multiple platforms correctly ensure that you use
  /// `GetPathToResource` to get the full path to the files in the projects 
  /// resources folder.
  ///
  /// LoadMusic can load mp3, wav and ogg audio files.
  ///
  /// `FreeMusic` should be called to free the resources used by the 
  /// `Music` data once the resource is no longer needed.
  ///
  ///@param path the path to the music file to load.
  ///
  ///@lib LoadMusic
  ///@uname LoadMusic
  ///
  ///@class Music
  ///@constructor
  function LoadMusic(path: String): Music;

  /// Frees the resources used by a `Music` resource. All loaded
  /// `Music` should be freed once it is no longer needed. 
  ///
  ///@lib FreeMusic
  ///@uname FreeMusic
  ///
  ///@class Music
  ///@dispose
  procedure FreeMusic(var mus: Music);

  /// Frees the resources used by a `SoundEffect` resource. All loaded
  /// `SoundEffect`s should be freed once it is no longer needed.
  ///
  ///@lib FreeSoundEffect
  ///@uname FreeSoundEffect
  ///
  ///@class SoundEffect
  ///@dispose
  procedure FreeSoundEffect(var effect: SoundEffect);

  /// There are several versions of PlaySoundEffect that can be used to control
  /// the way the sound effect plays, allowing you to control its volume and 
  /// the number of times the code loops. In all cases the started sound effect
  /// is mixed with the currently playing sound effects and music.
  ///
  /// With this version of PlaySoundEffect, the started sound effect will be 
  /// played at full volume.
  ///
  ///@param effect The effect indicates which sound effect to start playing. This
  ///               effect is played once at its full volume.
  ///
  ///@lib PlaySoundEffect
  ///@uname PlaySoundEffect
  ///
  ///@class SoundEffect
  ///@method Play
  procedure PlaySoundEffect(effect: SoundEffect); overload;
    
  /// This version of PlaySoundEffect allows you to indicate the number of times
  /// the sound effect is repeated. Setting the loops parameter to -1 will cause
  /// the sound effect to be looped infinitely, setting it to a value larger than
  /// 0 repeats the sound effect the number of times indicated. This means that 
  /// setting loops to 1 causes the `SoundEffect` to be played twice.
  /// 
  ///@param effect The effect indicates which sound effect to start playing. This
  ///               effect is played once at its full volume.
  ///
  ///@param loops Controls the number of times the sound effect is played.
  ///
  ///@lib PlaySoundEffectLoop
  ///@uname PlaySoundEffectLoop
  ///
  ///@class SoundEffect
  ///@overload Play PlayWithLoops
  procedure PlaySoundEffect(effect: SoundEffect; loops: LongInt); overload;
        
  /// This version of PlaySoundEffect allows you to control the volume of the 
  /// sounds playback. The vol parameter will take a value between 0 and 1 indicating
  /// the percentage of full volume to play at, for example, 0.1 playes the sound effect
  /// at 10% of its original volume.
  ///
  ///@param effect The effect indicates which sound effect to start playing. 
  ///@param vol Indicates the percentage of the original volume to play the 
  ///            `SoundEffect` at. This must be between 0 and 1.
  ///
  ///@lib PlaySoundEffectLoopVolume(effect, 0, vol)
  ///@uname PlaySoundEffectWithVolume
  ///@version 2.1
  ///
  ///@class SoundEffect
  ///@overload Play PlayWithVolume
  procedure PlaySoundEffect(effect: SoundEffect; vol: Single); overload;
  
  /// This version of PlaySoundEffect allows you to control both the number
  /// of times the `SoundEffect` is repeated, and its playback volume.
  ///
  ///@param effect The effect indicates which sound effect to start playing. 
  ///@param loops Controls the number of times the sound effect is played.
  ///@param vol Indicates the percentage of the original volume to play the 
  ///            `SoundEffect` at. This must be between 0 and 1.
  ///
  ///@lib PlaySoundEffectLoopVolume
  ///@uname PlaySoundEffectWithLoopAndVolume
  ///@version 2.0
  ///
  ///@class SoundEffect
  ///@overload Play PlayWithLoopsAndVolume
  procedure PlaySoundEffect(effect: SoundEffect; loops: LongInt; vol: Single); overload;

  /// PlayMusic starts playing a `Music` resource. SwinGame only allows one music resource 
  /// to be played at a time. Starting to play a new music resource will stop the currently playing music track.
  /// You can also stop the music by calling `StopMusic`.
  ///
  /// By default SwinGame starts playing the music at its full volume. This can be controlled by calling 
  /// `SetMusicVolume`. The current volume can be checked with `MusicVolume`.
  ///
  /// To test if a `Music` resource is currently playing you can use the `IsMusicPlaying` function.
  ///
  /// This version of PlayMusic can be used to play background music that is looped infinitely.
  /// The currently playing music is stopped and the new music resource will start playing, and will
  /// repeat until `StopMusic` is called, or another resource is played. 
  ///
  ///@param mus The `Music` resource to play.
  ///
  ///@lib PlayMusic(mus, -1)
  ///@uname PlayMusic
  ///
  ///@class Music
  ///@method Play
  procedure PlayMusic(mus: Music); overload;

  /// This version of PlayMusic allows you to control the number of times the `Music` 
  /// resource is repeated. It starts playing the supplied `Music` resource, repeating it the numder of times
  /// specified in the loops parameter. Setting loops to 0 plays the music through once. Setting 
  /// loops to -1 repeats the music infinitely, other values larger than 0 indicate the number of 
  /// times that the music should be repeated.
  ///
  ///@param mus The `Music` resource to be played.
  ///@param loops The number of times that the music should be repeated, for example, setting
  ///              loops to 1 causes the music to be played through twice.
  ///
  ///@lib PlayMusic
  ///@uname PlayMusicWithLoops
  ///
  ///@class Music
  ///@overload Play PlayWithLoops
  procedure PlayMusic(mus: Music; loops: LongInt); overload;

  /// This procedure allows you to set the volume of the currently playing music. The vol
  /// parameter indicates the percentage of the original volume, for example, 0.1 sets the
  /// playback volume to 10% of its full volume.
  ///
  ///@param vol Indicates the percentage of the original volume to play the 
  ///            `Music` at. This must be between 0 and 1, e.g. 0.1 is 10%.
  ///
  ///@lib SetMusicVolume
  ///@uname SetMusicVolume
  ///
  ///@class Music
  ///@setter Volume
  procedure SetMusicVolume(vol: Single);

  /// This function returns the current volume of the music. This will be
  /// a value between 0 and 1, with 1 indicating 100% of the `Music` resources
  /// volume.
  ///
  ///@returns The volume of the currently playing music.
  ///
  ///@lib MusicVolume
  ///@uname MusicVolume
  ///
  ///@class Music
  ///@getter Volume
  function MusicVolume(): Single;

  /// This function indicates if music is currently playing. As only one music 
  /// resource can be playing at a time this does not need to be told which
  /// music resource to check for.
  ///
  ///@returns true if the music is playing
  ///
  ///@lib IsMusicPlaying
  ///@uname IsMusicPlaying
  ///
  ///@class Music
  ///@static IsPlaying
  function IsMusicPlaying(): Boolean;
  
  /// This function can be used to check if a sound effect is currently 
  /// playing. 
  ///
  ///@param effect The sound effect to check.
  ///@returns true if the effect `SoundEffect` is playing.
  ///
  ///@lib IsSoundEffectPlaying
  ///@uname IsSoundEffectPlaying
  ///
  ///@class SoundEffect
  ///@method IsPlaying
  function IsSoundEffectPlaying(effect: SoundEffect): Boolean;

  /// Stops all occurances of the effect `SoundEffect` that is currently playing.
  ///
  ///@param effect The sound to stop.
  ///
  ///@lib StopSoundEffect
  ///@uname StopSoundEffect
  ///
  ///@class SoundEffect
  ///@method Stop
  procedure StopSoundEffect(effect: SoundEffect);

  /// Stops playing the current music resource.
  ///
  ///@lib StopMusic
  ///@uname StopMusic
  ///
  ///@class Music
  ///@static Stop
  procedure StopMusic();

implementation
  uses SysUtils, Classes;
       
  var
    // Contains the sound channels used to determine if a sound is currently
    // playing and enables us to stop the sound, check if it is playing etc.
    soundChannels: Array[0..7] of Pointer;

  //*******
  //
  // Sound routines
  //
  //*******

  procedure OpenAudio();
  begin
    {$ifdef DARWIN}
    if Mix_OpenAudio( 22050, MIX_DEFAULT_FORMAT, 2, 1024 ) = -1 then
    {$else}
    if Mix_OpenAudio( 22050, MIX_DEFAULT_FORMAT, 2, 1024 ) = -1 then      
    {$endif}
    begin
      raise Exception.Create('Error opening audio device: ' + string(Mix_GetError()));
    end;
  end;
  
  procedure CloseAudio();
  begin
    Mix_CloseAudio();
  end;
  
  /// Loads a sound effect from the file system. The sound effect can be in the
  /// form of a wav, ogg, or mp3 file.
  ///
  /// @param path     The path to the file to load
  /// @returns         The sound effect
  function LoadSoundEffect(path: String): SoundEffect;
  begin
    result := Mix_LoadWAV(pchar(path));
    if result = nil then
    begin
      raise Exception.Create('Error loading sound effect: ' + SDL_GetError());
    end;
  end;
  
  /// Load music to play from the file system. Music can be in the form of a
  /// wav, ogg, or mp3 file.
  ///
  /// @param path     The path to the file to be loaded
  /// @returns         The loaded music value
  function LoadMusic(path: String): Music;
  begin
    result := Mix_LoadMUS(pchar(path));
    if result = nil then
    begin
      raise Exception.Create('Error loading sound effect: ' + SDL_GetError());
    end;
  end;
  
  /// Free a sound effect. All loaded sound effects need to be freed.
  ///
  /// @param effect   The effect to be freed.
  ///
  /// Side Effect:
  /// - Frees the sound effect
  procedure FreeSoundEffect(var effect: SoundEffect);
  begin
    Mix_FreeChunk(effect);
    effect := nil;
  end;
  
  /// Free a music value. All loaded music values need to be freed.
  ///
  /// @param mus     The musi to be freed
  ///
  /// Side Effect:
  /// - Music is freed
  procedure FreeMusic(var mus: Music);
  begin
    Mix_FreeMusic(mus);
    mus := nil;
  end;
  
  procedure SetMusicVolume(vol: Single);
  var
    newVol: LongInt;
  begin
    if (vol < 0) then vol := 0
    else if vol > 1 then vol := 1;

    newVol := Trunc(vol * 128);
    
    //WriteLn('newVol ', vol:4:2, ' = ', newVol);
    
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
    if not Assigned(effect) then raise Exception.Create('Sound not supplied');
    if (vol < 0) then vol := 0
    else if vol > 1 then vol := 1;
    
    i := Mix_PlayChannel( -1, effect, loops);
    if i <> -1 then
    begin
      Mix_Volume(i, Trunc(vol * 128));
      soundChannels[i] := effect;
    end;
  end;
  
  /// Play the indicated sound effect a number of times.
  ///
  /// @param effect    the effect to play
  /// @param loops      The number of times to play it
  procedure PlaySoundEffect(effect: SoundEffect; loops: LongInt); overload;
  begin
   PlaySoundEffect(effect, loops, 1.0)
  end;
  
  /// Play the indicated sound effect once.
  ///
  /// @param effect    the effect to play
  procedure PlaySoundEffect(effect: SoundEffect); overload;
  begin
    PlaySoundEffect(effect, 0, 1.0);
  end;
  
  procedure PlaySoundEffect(effect: SoundEffect; vol: Single); overload;
  begin
   PlaySoundEffect(effect, 0, vol);
  end;
  
  /// Start the indicating music playing. The music will loop the indicated
  /// number of times.
  ///
  /// @param mus       The music to begin to play
  /// @param loops     The number of times to loop the music
  ///
  /// Side Effect:
  procedure PlayMusic(mus: Music; loops: LongInt); overload;
  begin
    Mix_HaltMusic();

    if not Assigned(mus) then raise Exception.Create('Music not supplied');
    Mix_PlayMusic(mus, loops);
  end;
  
  /// Start the indicating music playing. The music will continue to play
  /// until stopped.
  ///
  /// @param mus       The music to begin to play
  ///
  /// Side Effect:
  procedure PlayMusic(mus: Music); overload;
  begin
    PlayMusic(mus, -1);
  end;

  function IsSoundPlaying(effect: Pointer): Boolean;
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

  /// Determines if a sound effect is playing
  ///
  /// @param effect   The sound effect check if playing
  /// @return         True if the sound effect is playing
  function IsSoundEffectPlaying(effect: SoundEffect): Boolean;
  begin
    result := IsSoundPlaying(effect);
  end;

  /// Determines if the indicated music is playing.
  ///
  /// @param mus   The music to check if playing
  /// @returns     True if the music is playing
  function IsMusicPlaying(): Boolean;
  begin
    result := Mix_PlayingMusic() <> 0;
  end;

  /// Stop music from playing.
  ///
  /// Side Effects:
  /// - Stops the currently playing music
  procedure StopMusic();
  begin
    Mix_HaltMusic();
  end;

  /// Stop playing sound effects
  ///
  /// @param effect:   The sound effect to be stopped
  ///
  /// Side Effects:
  /// - The sound stops playing
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
end.
