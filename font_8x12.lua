--
-- Reads bmp->raw file  24-bits=(B,G,R)
--
-- Prepares data file output .asm as assembler text and .bin as binary version

filename = "font_8x12.bmp"

--
--
PW = {};
PG = {};
PR = {};
c = 16 * 6; -- letter

CR = string.char(13) .. string.char(10);

fname = string.sub( filename, 1, string.len(filename)-4 );

in_file = assert(io.open(filename, "rb"));
ou_file = assert(io.open(fname..".asm", "wb"));
ou_filb = assert(io.open(fname..".bin", "wb"));

in_file:read(54);	-- skip header
k=0;
r=0;      -- which row
pr=0;	  -- per row counter 

b=256;    -- 2^8
while true do
    local buf = in_file:read(3);
    if not buf then break end;
    R = string.byte(buf,1);
    G = string.byte(buf,2);
    B = string.byte(buf,3);
    Black = (R<2 and G<2 and B<2);
    White = (R>200 and G>200 and B>200);
    Red   = (R>200 and G<2 and B<2);
    Green = ((not White) and (not Black) and (not Red));

    k = k+1;
    b = b/2;


    if(b==128) then
        pr = pr + 1;
        if(pr==1) then
          r = r + 1;
	end
        c = c + 1;
        if(r==1) then
	  PW[c] = {};
	  PG[c] = {};
	  PR[c] = {};
        end
	PW[c][r] = ",";
	PG[c][r] = ",";
	PR[c][r] = ",";
    end


    if(White) then
       PW[c][r] = PW[c][r]..tonumber(b)..",";
    end
    if(Green) then
       PG[c][r] = PG[c][r]..tonumber(b)..",";
    end
    if(Red) then
       PR[c][r] = PR[c][r]..tonumber(b)..",";
    end

    if(b==1) then
       b=256;
       if(pr==16) then
         pr = 0;
         if(r<12) then
           c = c-16;
         else
           if(r==12) then
             r = 0;
             c = c-32;
           end
         end
       end
    end

end

-- 3 arrays of colours, background colour
-- Result = datas for assembler

function dispArr(arr1, arr2, arr3, col1, col2, col3, colBg)

 local ARR1, ARR2, ARR3;
 local s = "";

 local Ni = 0;
 local cd = 32;
 local pr = 0;

 while Ni<table.getn(arr1) do		-- scan all letters
  Ni = Ni + 1;
  ARR1 = arr1[Ni];
  ARR2 = arr2[Ni];
  ARR3 = arr3[Ni];

  s = s .. "    .by ";

  local k=table.getn(ARR1);
  local ww = 0;

  while k>0 do		-- scan all rows
    local D1 = ARR1[k];
    local D2 = ARR2[k];
    local D3 = ARR3[k];

    local by={};
    local b=256;
    while b>1 do
      b = b/2;
      local f1 = string.find (D1, ","..tonumber(b).."," );
      local f2 = string.find (D2, ","..tonumber(b).."," );
      local f3 = string.find (D3, ","..tonumber(b).."," );
      local col=colBg;

      if( f1~=nil or f2~=nil or f3~=nil ) then
        if(f1~=nil) then
          col=col1;
        end
        if(f2~=nil) then
          col=col2;
        end
        if(f3~=nil) then
          col=col3;
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

      if( ww==1 ) then
         s = s .. ", ";
      else
         ww = 1;
      end

      if(nb<10) then
        s = s .. string.format("%d",nb);
      else
        s = s .. "$".. string.format("%x",nb);
      end

      ou_filb:write( string.char(nb) );

      j = j - 1;

    end
    k = k - 1;

    if(k==0) then
      s = s .. "  ; chr(" .. cd .. ") '" .. string.char(cd) .. "'" .. CR;
    end

  end

  cd = cd + 1;
  pr = pr + 1;

  if(cd>131) then break end;

  if(pr==16) then
    pr = 0;
  end


 end
 return s;

end


function toFile(txt)

-- print out assembler data text

-- colours 1-zils
-- 0=black
-- 1=light red
-- 2=light green
-- 3=light blue

ou_file:write(txt .. dispArr(PW,PG,PR,2,1,3,0) .. CR);
end


toFile("");



ou_file:close()
ou_filb:close()
in_file:close()
print("Ok");

