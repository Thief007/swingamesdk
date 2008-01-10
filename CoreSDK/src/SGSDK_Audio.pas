unit SGSDK_Audio;

{$IFDEF UNIX}
	{$linklib gcc}
	{$linklib SDLmain}
{$ENDIF}

interface
	uses SDL_Mixer, SDL, SDL_Image, SDL_TTF, SDLEventProcessing, SGSDK_Core;
	
	type
		/// Type: SoundEffect
		///
		/// Use sound effects to play sounds in a game. Load sound effects using th
		///	LoadSoundEffect routine. You can then PlaySoundEffect to start the
		///	effect playing.
		///
		/// NOTE: Do not use this to play music, see the Music type.
		SoundEffect = PMix_Chunk;

		/// Type: Music
		///
		/// Background music is played on a loop. Use this music type
		/// for variables that refer to music that can be played. You can load
		///	these using LoadMusic, play with PlayMusic, and stop with StopMusic.
		///	Also see the IsMusicPlaying routine.
		Music = PMix_Music;

	//*****
	//
	// Sound routines
	//
	//*****
	//
	// These routines are used to work with sound effects and music within the
	// game API.
	//
	procedure OpenAudio();
	procedure CloseAudio();

	function LoadSoundEffect(path: String): SoundEffect;

	function LoadMusic(path: String): Music;

	procedure FreeMusic(var mus: Music);

	procedure FreeSoundEffect(var effect: SoundEffect);

	procedure PlaySoundEffect(effect: SoundEffect); overload;

	procedure PlaySoundEffect(effect: SoundEffect; loops: Integer); overload;

	procedure PlayMusic(mus: Music; loops: Integer); overload;

	procedure PlayMusic(mus: Music); overload;

	function IsMusicPlaying(): Boolean;

	function IsSoundEffectPlaying(effect: SoundEffect): Boolean;

	procedure StopSoundEffect(effect: SoundEffect);

	procedure StopMusic();

implementation
	uses SysUtils, Math, Classes;
			 
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
		//WriteLn('Opening Mixer');
		{$ifdef UNIX}
		if Mix_OpenAudio( 22050, MIX_DEFAULT_FORMAT, 2, 4096 ) = -1 then
		{$else}
		if Mix_OpenAudio( 22050, MIX_DEFAULT_FORMAT, 2, 1024 ) = -1 then			
		{$endif}
		begin
			WriteLn('Errorm loading mixer...');
			WriteLn(string(Mix_GetError));
			RaiseSGSDKException('Error openning audio device. ' + string(Mix_GetError));
		end;
		//WriteLn('Mixer Open');
	end;
	
	procedure CloseAudio();
	begin
		//WriteLn('Closing Audio');
		try
			Mix_CloseAudio();
		except
			RaiseSGSDKException('Failed to close audio');
		end;
		//WriteLn('Closed Audio');	
	end;
	
	/// Loads a sound effect from the file system. The sound effect can be in the
	///	form of a wav, ogg, or mp3 file.
	///
	///	@param path			The path to the file to load
	///	@returns				 The sound effect
	function LoadSoundEffect(path: String): SoundEffect;
	begin
		result := Mix_LoadWAV(pchar(path));
		if result = nil then
		begin
			RaiseSGSDKException('Error loading sound effect: ' + SDL_GetError());
		end;
	end;
	
	/// Load music to play from the file system. Music can be in the form of a
	///	wav, ogg, or mp3 file.
	///
	///	@param path			The path to the file to be loaded
	///	@returns				 The loaded music value
	function LoadMusic(path: String): Music;
	begin
		result := Mix_LoadMUS(pchar(path));
		if result = nil then
		begin
			RaiseSGSDKException('Error loading sound effect: ' + SDL_GetError());
		end;
	end;
	
	/// Free a sound effect. All loaded sound effects need to be freed.
	///
	///	@param effect		The effect to be freed.
	///
	/// Side Effect:
	///	- Frees the sound effect
	procedure FreeSoundEffect(var effect: SoundEffect);
	begin
		try
			Mix_FreeChunk(effect);
			effect := nil;
		except
			RaiseSGSDKException('Failed to free the specified sound effect');
		end;
	end;
	
	/// Free a music value. All loaded music values need to be freed.
	///
	///	@param mus		 The musi to be freed
	///
	/// Side Effect:
	///	- Music is freed
	procedure FreeMusic(var mus: Music);
	begin
		try
			Mix_FreeMusic(mus);
			mus := nil;
		except
			RaiseSGSDKException('Failed to free the specified music');
		end;
	end;
	
	/// Play the indicated sound effect a number of times.
	///
	///	@param effect		 the effect to play
	///	@param loops			The number of times to play it
	procedure PlaySoundEffect(effect: SoundEffect; loops: Integer); overload;
	var
		i: Integer;
	begin
		try
			i := Mix_PlayChannel( -1, effect, loops );
			if i <> -1 then soundChannels[i] := effect;
		except
			RaiseSGSDKException('Failed to play the specified sound effect');
		end;
	end;
	
	/// Play the indicated sound effect once.
	///
	///	@param effect		 the effect to play
	procedure PlaySoundEffect(effect: SoundEffect); overload;
	begin
		PlaySoundEffect(effect, 0);
	end;
	
	/// Start the indicating music playing. The music will loop the indicated
	///	number of times.
	///
	///	@param mus			 The music to begin to play
	///	@param loops		 The number of times to loop the music
	///
	/// Side Effect:
	procedure PlayMusic(mus: Music; loops: Integer); overload;
	var
		i: Integer;
	begin
		try
			Mix_HaltMusic();

			i := Mix_PlayMusic(mus, loops);
			if i <> -1 then soundChannels[i] := mus;

			//i := Mix_PlayMusic(mus, loops);
		except
			RaiseSGSDKException('Failed to play the specified music');
		end;
	end;
	
	/// Start the indicating music playing. The music will continue to play
	///	until stopped.
	///
	///	@param mus			 The music to begin to play
	///
	/// Side Effect:
	procedure PlayMusic(mus: Music); overload;
	begin
		PlayMusic(mus, -1);
	end;

	function IsSoundPlaying(effect: Pointer): Boolean;
	var
		i: Integer;
	begin
		result := false;

		try
			for i := 0 to High(soundChannels) do
			begin
				if soundChannels[i] = effect then
				begin
					result := Mix_Playing(i) <> 0;
					break;
				end;
			end;
		except
			RaiseSGSDKException('Failed to check if the specified sound effect is playing');
		end;
	end;

	/// Determines if a sound effect is playing
	///
	///	@param effect		The sound effect check if playing
	///	@return					True if the sound effect is playing
	function IsSoundEffectPlaying(effect: SoundEffect): Boolean;
	begin
		result := IsSoundPlaying(effect);
	end;

	/// Determines if the indicated music is playing.
	///
	///	@param mus	 The music to check if playing
	///	@returns		 True if the music is playing
	function IsMusicPlaying(): Boolean;
	begin
		try
			result := Mix_PlayingMusic() <> 0;
		except
      result := false;
			RaiseSGSDKException('Failed to check if the music is playing');
		end;
	end;

	/// Stop music from playing.
	///
	/// Side Effects:
	///	- Stops the currently playing music
	procedure StopMusic();
	begin
		try
			Mix_HaltMusic();
		except
			RaiseSGSDKException('Failed to halt music');
		end;
	end;

	/// Stop playing sound effects
	///
	///	@param effect:	 The sound effect to be stopped
	///
	/// Side Effects:
	///	- The sound stops playing
	procedure StopSoundEffect(effect: SoundEffect);
	var
		i: Integer;
	begin
		for i := 0 to High(soundChannels) do
		begin
			if soundChannels[i] = effect then
			begin
				try
					Mix_HaltChannel(i);
				except
					RaiseSGSDKException('Failed to halt the specified sound effect');
				end;
				break;
			end;
		end;
	end;
end.
