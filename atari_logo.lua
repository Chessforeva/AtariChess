--
-- Reads bmp->raw file  24-bits=(B,G,R)
--
-- Prepares data file output .asm as assembler text and .bin as binary version

filename = "atari_logo.bmp";

--
--
PR = {};

c = 4 * 3;

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
while k< (8 * 4 * 8 * 4) do
    local buf = in_file:read(3);
    if not buf then break end;
    R = string.byte(buf,1);
    G = string.byte(buf,2);
    B = string.byte(buf,3);
    --Black = (R<2 and G<2 and B<2);
    --White = (R>200 and G>200 and B>200);
    --Red   = (R>200 and G<2 and B<2);

    Black = (B<2);
    Red = (B>200);

    k = k+1;
    b = b/2;


    if(b==128) then
        pr = pr + 1;
        if(pr==1) then
          r = r + 1;
	end
        c = c + 1;
        if(r==1) then
	  PR[c] = {};
        end
	PR[c][r] = ",";
    end


    if(Red) then
       PR[c][r] = PR[c][r]..tonumber(b)..",";

    end

    if(b==1) then
       b=256;
       if(pr==4) then
         pr = 0;
         if(r<8) then
           c = c-4;
         else
           if(r==8) then
             r = 0;
             c = c-8;
           end
         end
       end
    end

end

-- Array of Colours, background colour
-- Result = datas for assembler

function dispArr(arr1, col1, colBg)

 local ARR1;
 local s = "";

 local Ni = 0;
 local q = 1;
 local pr = 0;


 while Ni<4 do		-- scan 4 times 4 logo parts
  Ni = Ni + 1;  
 
  local k=0;
  while k<8 do		-- scan 8 rows
   k = k + 1;

   s = s .. "    .by ";

   local Nj=0;
   while Nj<4 do	-- for next 4 logo parts
    Nj = Nj + 1;

    local D1 = arr1[ ((Ni-1)*4) + Nj ][9-k];

    local by={};
    local b=256;
    while b>1 do
      b = b/2;
      local f1 = string.find (D1, ","..tonumber(b).."," );
      local col=colBg;

      if( f1~=nil ) then
        if(f1~=nil) then
          col=col1;
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

    end --j<2

   end --Nj<4

   s = s .. CR;
   ww=0;

  end --k<8


 end --Ni<4

 return s;

end


function toFile(txt)

-- print out assembler data text

-- colours 1-zils
-- 0=black
-- 1=light red
-- 2=light green
-- 3=light blue

ou_file:write(txt .. dispArr(PR,1,0) .. CR);
end


toFile("");



ou_file:close()
ou_filb:close()
in_file:close()
print("Ok");

