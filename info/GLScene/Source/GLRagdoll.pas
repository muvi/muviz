{: GLRagdoll<p>

	Base abstract ragdoll class. Should be extended to use any physics system. <p>

	<b>History :</b><font size=-1><ul>
    <li>09/11/05 - LucasG - Fixed joint and few small things
    <li>07/11/05 - LucasG - Fixed bone position and rotation (Align to animation)
    <li>02/11/05 - LucasG - First version created.
  </ul></font>
}

unit GLRagdoll;

interface

uses GLScene, PersistentClasses, VectorGeometry, GLVectorFileObjects,
  VectorLists, GLObjects;

type
  TGLRagdoll = class;
  TRagdollBone = class;

  TRagdollJoint = class
  end;

  TRagdollBoneList = class (TPersistentObjectList)
  private
    { Private Declarations }
     FRagdoll : TGLRagdoll;
  protected
    { Protected Declarations }
    function GetRagdollBone(Index: Integer) : TRagdollBone;
  public
    { Public Declarations }
    constructor Create(Ragdoll: TGLRagdoll);
    destructor Destroy; override;

    procedure WriteToFiler(writer : TVirtualWriter); override;
    procedure ReadFromFiler(reader : TVirtualReader); override;

    property Ragdoll : TGLRagdoll read FRagdoll;
    property Items[Index: Integer] : TRagdollBone read GetRagdollBone; default;
	end;

	TRagdollBone = class (TRagdollBoneList)
  private
    { Private Declarations }
    FOwner : TRagdollBoneList;
    FName : String;
    FBoneID : Integer; //Refering to TGLActor Bone
    FBoundMax: TAffineVector;
    FBoundMin: TAffineVector;
    FBoundBoneDelta: TAffineVector; //Stores the diference from the bone.GlobalMatrix to the center of the bone's bounding box
    FOrigin: TAffineVector;
    FSize: TAffineVector;
    FBoneMatrix: TMatrix;
    FJoint: TRagdollJoint;
    FOriginalMatrix: TMatrix; //Stores the Bone.GlobalMatrix before the ragdoll start
    FReferenceMatrix: TMatrix; //Stores the first bone matrix to be used as reference
    FAnchor: TAffineVector; //The position of the joint
    procedure CreateBoundingBox;
    procedure SetAnchor(Anchor: TAffineVector);
    procedure AlignToSkeleton;
    procedure CreateBoundsChild;
    procedure StartChild;
    procedure AlignChild;
    procedure UpdateChild;
    procedure StopChild;
  protected
    { Protected Declarations }
    function GetRagdollBone(Index: Integer) : TRagdollBone;
    procedure Start; virtual; abstract;
    procedure Align; virtual; abstract;
    procedure Update; virtual; abstract;
    procedure Stop; virtual; abstract;
  public
    { Public Declarations }
    constructor CreateOwned(aOwner : TRagdollBoneList);
    constructor Create(Ragdoll: TGLRagdoll);
    destructor Destroy; override;

    procedure WriteToFiler(writer : TVirtualWriter); override;
    procedure ReadFromFiler(reader : TVirtualReader); override;

    property Owner : TRagdollBoneList read FOwner;
    property Name : String read FName write FName;
    property BoneID : Integer read FBoneID write FBoneID;
    property Origin : TAffineVector read FOrigin;
    property Size : TAffineVector read FSize;
    property BoneMatrix : TMatrix read FBoneMatrix;
    property ReferenceMatrix : TMatrix read FReferenceMatrix;
    property Anchor : TAffineVector read FAnchor;
    property Joint : TRagdollJoint read FJoint write FJoint;
    property Items[Index: Integer] : TRagdollBone read GetRagdollBone; default;
	end;

  TGLRagdoll = class(TPersistentObject)
	private
    { Private Declarations }
    FOwner : TGLBaseMesh;
    FRootBone : TRagdollBone;
    FEnabled: Boolean;
    FBuilt: Boolean;
  protected
    { Protected Declarations }
  public
    { Public Declarations }
    constructor Create(AOwner : TGLBaseMesh);
    destructor Destroy; override;

    procedure WriteToFiler(writer : TVirtualWriter); override;
    procedure ReadFromFiler(reader : TVirtualReader); override;

    {: Must be set before build the ragdoll }
    procedure SetRootBone(RootBone: TRagdollBone);
    {: Create the bounding box and setup the ragdoll do be started later }
    procedure BuildRagdoll;

    procedure Start;
    procedure Update;
    procedure Stop;

    property Owner : TGLBaseMesh read FOwner;
    property RootBone : TRagdollBone read FRootBone;
    property Enabled : Boolean read FEnabled;
	end;

implementation

{ TRagdollBoneList }

constructor TRagdollBoneList.Create(Ragdoll: TGLRagdoll);
begin
  inherited Create;
  FRagdoll := Ragdoll;
end;

destructor TRagdollBoneList.Destroy;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Destroy;
  inherited;
end;

function TRagdollBoneList.GetRagdollBone(Index: Integer): TRagdollBone;
begin
  Result:=TRagdollBone(List^[Index]);
end;

procedure TRagdollBoneList.ReadFromFiler(reader: TVirtualReader);
begin
  inherited;
  //Not implemented
end;

procedure TRagdollBoneList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited;
  //Not implemented
end;

{ TRagdollBone }

constructor TRagdollBone.Create(Ragdoll: TGLRagdoll);
begin
  inherited Create(Ragdoll);
end;

procedure TRagdollBone.CreateBoundingBox;
var
  bone: TSkeletonBone;
  i, j: integer;
  BoneVertices : TAffineVectorList;
  BoneVertex, max,min: TAffineVector;
  invMat, mat: TMatrix;
begin
  bone := Ragdoll.Owner.Skeleton.BoneByID(FBoneID);

  //Get all vertices weighted to this bone
  BoneVertices:=TAffineVectorList.Create;
  for i:=0 to Ragdoll.Owner.MeshObjects.Count-1 do
  with TSkeletonMeshObject(Ragdoll.Owner.MeshObjects[i]) do
    for j:=0 to Vertices.Count-1 do
      if bone.BoneID = VerticesBonesWeights[j][0].BoneID then
        BoneVertices.FindOrAdd(Vertices[j]);

  invMat := bone.GlobalMatrix;
  InvertMatrix(invMat);

  //For each vertex, get the max and min XYZ (Bounding box)
  if BoneVertices.Count > 0 then
  begin
    BoneVertex := VectorTransform(BoneVertices[0], invMat);
    max := BoneVertex;
    min := BoneVertex;
    for i:=1 to BoneVertices.Count-1 do begin
      BoneVertex := VectorTransform(BoneVertices[i], invMat);
      if (BoneVertex[0] > max[0]) then max[0] := BoneVertex[0];
      if (BoneVertex[1] > max[1]) then max[1] := BoneVertex[1];
      if (BoneVertex[2] > max[2]) then max[2] := BoneVertex[2];

      if (BoneVertex[0] < min[0]) then min[0] := BoneVertex[0];
      if (BoneVertex[1] < min[1]) then min[1] := BoneVertex[1];
      if (BoneVertex[2] < min[2]) then min[2] := BoneVertex[2];
    end;

    FBoundMax := max;
    FBoundMin := min;
    //Get the origin and subtract from the bone matrix
    FBoundBoneDelta := VectorScale(VectorAdd(FBoundMax, FBoundMin), 0.5);
  end else begin
    FBoundMax := NullVector;
    FBoundMin := NullVector;
  end;

  AlignToSkeleton;
  FReferenceMatrix := FBoneMatrix;
  mat := MatrixMultiply(bone.GlobalMatrix,FRagdoll.Owner.AbsoluteMatrix);
  //Set Joint position
  SetAnchor(AffineVectorMake(mat[3]));

  BoneVertices.Free; // NEW1
end;

constructor TRagdollBone.CreateOwned(aOwner: TRagdollBoneList);
begin
	Create(aOwner.Ragdoll);
  FOwner:=aOwner;
  aOwner.Add(Self);
end;

destructor TRagdollBone.Destroy;
begin
  inherited;
end;

procedure TRagdollBone.AlignToSkeleton;
var
  o: TAffineVector;
  bone: TSkeletonBone;
  mat, posMat: TMatrix;
  noBounds: Boolean;
begin
  bone := Ragdoll.Owner.Skeleton.BoneByID(FBoneID);
  noBounds := VectorIsNull(FBoundMax) and VectorIsNull(FBoundMin);
  //Get the bone matrix relative to the Actor matrix
  mat := MatrixMultiply(bone.GlobalMatrix,FRagdoll.Owner.AbsoluteMatrix);
  //Set Rotation
  FBoneMatrix := mat;
  NormalizeMatrix(FBoneMatrix);

  if (noBounds) then
  begin
    FOrigin := AffineVectorMake(mat[3]);
    FSize := AffineVectorMake(0.1,0.1,0.1);
  end else begin
    //Set Origin
    posMat := mat;
    posMat[3] := NullHmgVector;
    o := VectorTransform(FBoundBoneDelta, posMat);
    FOrigin := VectorAdd(AffineVectorMake(mat[3]), o);
    //Set Size
    FSize := VectorScale(VectorSubtract(FBoundMax, FBoundMin),0.9);
    FSize[0] := FSize[0]*VectorLength(mat[0]);
    FSize[1] := FSize[1]*VectorLength(mat[1]);
    FSize[2] := FSize[2]*VectorLength(mat[2]);
  end;
  //Put the origin in the BoneMatrix
  FBoneMatrix[3] := VectorMake(FOrigin,1);
end;

function TRagdollBone.GetRagdollBone(Index: Integer): TRagdollBone;
begin
  Result:=TRagdollBone(List^[Index]);
end;

procedure TRagdollBone.ReadFromFiler(reader: TVirtualReader);
begin
  inherited;

end;

procedure TRagdollBone.StartChild;
var i: integer;
begin
  FOriginalMatrix := Ragdoll.Owner.Skeleton.BoneByID(FBoneID).GlobalMatrix;
  AlignToSkeleton;
  Start;
  for i := 0 to Count-1 do items[i].StartChild;
end;

procedure TRagdollBone.UpdateChild;
var i: integer;
begin
  Update;
  for i := 0 to Count-1 do items[i].UpdateChild;
end;

procedure TRagdollBone.WriteToFiler(writer: TVirtualWriter);
begin
  inherited;

end;

procedure TRagdollBone.StopChild;
var i: integer;
begin
  Stop;
  Ragdoll.Owner.Skeleton.BoneByID(FBoneID).SetGlobalMatrix(FOriginalMatrix);
  for i := 0 to Count-1 do items[i].StopChild;
end;

procedure TRagdollBone.CreateBoundsChild;
var i: integer;
begin
  CreateBoundingBox;
  for i := 0 to Count-1 do items[i].CreateBoundsChild;
end;

procedure TRagdollBone.SetAnchor(Anchor: TAffineVector);
begin
  FAnchor := Anchor;
end;

procedure TRagdollBone.AlignChild;
var i: integer;
begin
  Align;
  Update;
  for i := 0 to Count-1 do items[i].AlignChild;
end;

{ TGLRagdoll }

constructor TGLRagdoll.Create(AOwner : TGLBaseMesh);
begin
  FOwner := AOwner;
  FEnabled := False;
  FBuilt := False;
end;

destructor TGLRagdoll.Destroy;
begin
  if FEnabled then Stop;
  inherited Destroy;
end;

procedure TGLRagdoll.ReadFromFiler(reader: TVirtualReader);
begin
  inherited;
end;

procedure TGLRagdoll.SetRootBone(RootBone: TRagdollBone);
begin
  FRootBone := RootBone;
end;

procedure TGLRagdoll.Start;
begin
  Assert(FBuilt, 'First you need to build the ragdoll. BuildRagdoll;');
  if (FEnabled) then Exit;
  FEnabled:= True;
  //First start the ragdoll in the reference position
  RootBone.StartChild;
  //Now align it to the animation
  RootBone.AlignChild;
  //Now it recalculate the vertices to use as reference
  FOwner.Skeleton.StartRagDoll;
end;

procedure TGLRagdoll.Update;
begin
  if FEnabled then
  begin
    RootBone.UpdateChild;
    FOwner.Skeleton.MorphMesh(true);
  end;
end;

procedure TGLRagdoll.Stop;
begin
  if not FEnabled then Exit;
  FEnabled := False;
  RootBone.StopChild;
  //Restore the old information
  FOwner.Skeleton.StopRagDoll;
  FOwner.Skeleton.MorphMesh(true);
end;

procedure TGLRagdoll.WriteToFiler(writer: TVirtualWriter);
begin
  inherited;

end;

procedure TGLRagdoll.BuildRagdoll;
begin
  Assert(RootBone <> nil, 'First you need to set the root bone. SetRootBone();');
  RootBone.CreateBoundsChild;
  FBuilt := True;
end;

end.
