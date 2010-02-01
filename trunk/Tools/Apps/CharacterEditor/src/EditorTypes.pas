unit EditorTypes;

//=============================================================================
interface
uses
  sgTypes, sgUserInterface;
	
const
	CellGap 								= 5;
	ScreenWidth 						= 800;
	ScreenHeight						= 600;
	LengthCellData 					= 6;
	LengthgrpData 					= 10;
	LengthMode							= 2;
	LengthAniEditing				= 6;

//Clip Area
	ClipX										= 195;
	ClipY										= 65;
	ClipW										= 570;
	ClipH										= 480;
	
//Cell Group Indexes
	cgXpos									= 0;
	cgYpos									= 1;
	cgDrag									= 2;
	cgCellCount							= 3;
	cgWidth									= 4;
	cgHeight								= 5;
	cgColumns								= 6;
	cgRows									= 7;
	cgScale									= 8;
	cgSelected							= 9;
//Animation Editing Area
	aeXpos									= 0;
	aeYpos									= 1;
	aeDrag									= 2;
	aeCellCount							= 3;
	aeWidth									= 4;
	aeHeight								= 5;

//Cell Area Indexes
	caXpos									= 0;
	caYpos									= 1;
	caCell									= 2;
	caXGap									= 3;
	caYGap									= 4;
	caBitmap								= 5;
	caID										= 4;
		
// Modes
	blankCells							= 0;
	splitImage							= 1;
//Colors
	Red											= 255;
	Green										= 255;
	Blue										= 255;
//Multipliers
	Neg											= -1;
	Pos											= 1;
//Other
	edgeDistance						= 50;
	ArrowWidth							= 39;
	ArrowHeight							= 26;
	
type
	LoadedBitmap = record
		original, src : Bitmap;
	end;
	
	CellArea = record
		data : Array [0..LengthCellData] of LongInt;
		isSelected : Boolean;
	end;
	
	CellGroup = record
		grpData : Array of LongInt; 
		cellArea : Array of CellArea;
		selectedOrder : Array of LongInt; 
		isAniGroup : boolean;
	end;
			
	Frame = record
		ids, cells, duration, next : Array of LongInt;
	end;
	
	InitialData = record
		name : string;
		startCell : LongInt;
	end;	
	
	DraggedCell = record
		index, bmp, cell:LongInt;
		isAniGroup: Boolean
	end;
	
	MultiFrame = Array of Frame;
	CellGroups 		= array of CellGroup;
	arrayofString = array [cgCellCount..cgScale] of String;
	arrayOfLoadedBMP = array of LoadedBitmap;
	editorPanels = Array [0..3] of Panel;

implementation
uses
	crt, sgCore, sgAudio, sgText, sgGraphics, sgResources,
  sgGeometry, sgImages, sgInput, sgPhysics, sgNamedIndexCollection,
  sgSprites, SysUtils, sgShared, sgAnimations;
	begin
	end.