--
-- Reads bmp->raw file  24-bits=(B,G,R)
--
-- Prepares data file output .asm as assembler text and .bin as binary version

filename = "fig24bit.bmp"

--
--

PcK = {};
PcQ = {};
PcR = {};
PcB = {};
PcN = {};
PcP = {};

PiK = {};
PiQ = {};
PiR = {};
PiB = {};
PiN = {};
PiP = {};


pK = 0;
pQ = 0;
pR = 0;
pB = 0;
pN = 0;
pP = 0;

CR = string.char(13) .. string.char(10);

fname = string.sub( filename, 1, string.len(filename)-4 );

in_file = assert(io.open(filename, "rb"));
ou_file = assert(io.open(fname..".asm", "wb"));
ou_filb = assert(io.open(fname..".bin", "wb"));

in_file:read(54);	-- skip header
k=0;
q=0;      -- which piece
b=256;    -- 2^8
while k<16*(16*6) do
    local buf = in_file:read(3);
    if not buf then break end;
    R = string.byte(buf,1);
    G = string.byte(buf,2);
    B = string.byte(buf,3);
    Black = (R<2 and G<2 and B<2);
    White = (R>200 and G>200 and B>200);
    Blue = ((not White) and (not Black));

    k = k+1;
    b = b/2;


    if(q>=0 and q<16) then


      if(b==128) then
        pK = pK + 1;
        PiK[pK] = ",";
        PcK[pK] = ",";
      end

      if(Black) then
        PiK[pK] = PiK[pK]..tonumber(b)..",";
      end
      if(Blue) then
        PcK[pK] = PcK[pK]..tonumber(b)..",";
      end

    end

    if(q>=16 and q<32) then

      if(b==128) then
        pQ = pQ + 1;
        PiQ[pQ] = ",";
        PcQ[pQ] = ",";
      end

      if(Black) then
        PiQ[pQ] = PiQ[pQ]..tonumber(b)..",";
      end
      if(Blue) then
        PcQ[pQ] = PcQ[pQ]..tonumber(b)..",";
      end

    end

    if(q>=32 and q<48) then

      if(b==128) then
        pR = pR + 1;
        PiR[pR] = ",";
        PcR[pR] = ",";
      end

      if(Black) then
        PiR[pR] = PiR[pR]..tonumber(b)..",";
      end
      if(Blue) then
        PcR[pR] = PcR[pR]..tonumber(b)..",";
      end

    end

    if(q>=48 and q<64) then

      if(b==128) then
        pB = pB + 1;
        PiB[pB] = ",";
        PcB[pB] = ",";
      end

      if(Black) then
        PiB[pB] = PiB[pB]..tonumber(b)..",";
      end
      if(Blue) then
        PcB[pB] = PcB[pB]..tonumber(b)..",";
      end

    end

    if(q>=64 and q<80) then

      if(b==128) then
        pN = pN + 1;
        PiN[pN] = ",";
        PcN[pN] = ",";
      end

      if(Black) then
        PiN[pN] = PiN[pN]..tonumber(b)..",";
      end
      if(Blue) then
        PcN[pN] = PcN[pN]..tonumber(b)..",";
      end

    end

    if(q>=80 and q<96) then

      if(b==128) then
        pP = pP + 1;
        PiP[pP] = ",";
        PcP[pP] = ",";
      end

      if(Black) then
        PiP[pP] = PiP[pP]..tonumber(b)..",";
      end
      if(Blue) then
        PcP[pP] = PcP[pP]..tonumber(b)..",";
      end

    end

    q = q+1;

    if(k%(16*6)==0) then
      q=0;
    end

    if(b==1) then
       b=256;
    end

end

-- Array of contour and intern colours, background colour
-- Result = datas for assembler

function dispArr(arr1, arr2, colC, colI, colBg)
  local cc = 0;
  local s = "";
  local i=1;
  local k=table.getn(arr1)-1;

  
  while i<=table.getn(arr1) do
    local D1 = arr1[k];
    local D2 = arr2[k];
 
    k = k + 1;
    if(k%2>0) then
      k = k-4;
    end

    local by={};
    local b=256;
    while b>1 do
      b = b/2;
      local f1 = string.find (D1, ","..tonumber(b).."," );
      local f2 = string.find (D2, ","..tonumber(b).."," );
      local col=colBg;

      if( f1~=nil or f2~=nil ) then
        if(f1~=nil) then
          col=colC;
        end
        if(f2~=nil) then
          col=colI;
        end
      end
      by[ table.getn(by)+1 ]=col;
    end

    local m=1;
    local j=2;
    while j>0 do
      b=256;
      local nb = 0;
      while b>1 do
        b = b/4;
        nb = nb + (b*by[m]);
        m = m+1;
      end

      if(cc%16 ~= 0) then
        s = s..",";
      end

      if(nb<10) then
        s = s .. string.format("%d",nb);
      else
        s = s .. "$".. string.format("%x",nb);
      end

      ou_filb:write( string.char(nb) );

      cc = cc + 1;
      if(cc%16 == 0) then
        s = s .. CR;
        if(cc<(16*4)) then
           s = s .. "    .by ";
        end
      end

      j=j-1
    end
 

    j=2

    i=i+1;
  end
  return s;

end


function toFile(txt, arr1, arr2)

-- print out assembler data text

-- colours 1-zils
-- 0=black
-- 1=light red
-- 2=light green

-- 3=light blue
ou_file:write(txt.."Wb .by " .. dispArr(arr1,arr2,2,1,0) .. CR);
ou_file:write(txt.."Bb .by " .. dispArr(arr1,arr2,2,0,0) .. CR);
ou_file:write(txt.."Ww .by " .. dispArr(arr1,arr2,2,1,3) .. CR);
ou_file:write(txt.."Bw .by " .. dispArr(arr1,arr2,2,0,3) .. CR);

end


toFile("King",PcK,PiK);
toFile("Queen",PcQ,PiQ);
toFile("Rook",PcR,PiR);
toFile("Bishop",PcB,PiB);
toFile("Knight",PcN,PiN);
toFile("Pawn",PcP,PiP);

ou_file:close()
ou_filb:close()
in_file:close()
print("Ok");

