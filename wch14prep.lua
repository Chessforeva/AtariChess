-- This prepares World Chess Championship PGN file datas

filename = "wch14.pgn";

-- this loads and executes other .lua file
function dofile (filename)
  local f = assert(loadfile(filename))
  return f()
end
dofile( "c0_chess_subroutine.lua" );	-- chess logic

function Alltrim(s)
  local i1,i2 = string.find(s,'^%s*');
  if i2 >= i1 then s = string.sub(s,i2+1) end;
  local i1,i2 = string.find(s,'%s*$');
  if i2 >= i1 then s = string.sub(s,1,i1-1) end;
  return s;
end

function GetFData(s)
  local i1 = string.find(s,'"');
  s = string.sub(s,i1+1);
  local i2 = string.find(s,'"');
  s = string.sub(s,1,i2-1);
  return s;
end



fname = string.sub( filename, 1, string.len(filename)-4 );

in_file = assert(io.open(filename, "rt"));
ou_file = assert(io.open(fname..".asm", "wt"));
ou_filb = assert(io.open(fname..".bin", "wb"));

CR = string.char(13) .. string.char(10);

s = 0;
P = "";
I = "";

while true do
 t = in_file:read();
 if not t then break end;
 t = Alltrim(t);
 if(string.len(t)>0) then
   if(s==0) then
     if(string.sub(t,1,1)=='[') then
        if( string.sub(t,1,7)=='[Round ') then
          Ir = GetFData(t);
        end
        if( string.sub(t,1,7)=='[White ') then
          IW = GetFData(t);
        end
        if( string.sub(t,1,7)=='[Black ') then
          IB = GetFData(t);
        end
        if( string.sub(t,1,8)=='[Result ') then
          Iz = GetFData(t);
          if( string.find(Iz,"0")==nil ) then
            Iz="1/2"
          end
        end
     else
        if( string.sub(t,1,2)=='1.') then
          s=1;
          I = "Round" .. Ir.." "..string.sub(IW,1,1) .. ".-" ..
                        string.sub(IB,1,1)..". "..Iz;
          P = t .. " ";
          ou_file:write( "Item".. Ir .. " .by " .. '"' .. I .. '", 0' .. CR);
          ou_filb:write(I);
          ou_filb:write(string.char(0));
        end
     end
   else
     if(s==1) then
       P = P .. t .. " ";
     end
   end

 else
   if(s==1) then
     s=0;
     c0_LuaChess.c0_set_start_position("");
     uci = c0_LuaChess.c0_get_moves_from_PGN(P);

     ou_file:write( "Game".. Ir .. CR );
     cnt = 0;

     -- analyse list of moves and prepare datas
     Mn = 0; wm = 0;
     i = 1;
     while i<string.len(uci) do

       if(wm==0) then
         Mn = Mn + 1;
       end
       if(cnt == 0) then
         ww=0;
         W="   .by ";
       end


       fH = string.byte(uci,i+0)-string.byte("a",1);
       fV = string.byte(uci,i+1)-string.byte("1",1);
       tH = string.byte(uci,i+2)-string.byte("a",1);
       tV = string.byte(uci,i+3)-string.byte("1",1);
       i = i + 4;
       d = 0;
       if( i<string.len(uci) ) then
         if( string.sub(uci,i,i)=='[') then
           g = string.sub(uci,i+1,i+1);
           i = i + 3;
           if( string.find( "QRBN", g )~=nil ) then
             d=1;
           end
         end
       end
       Cf = 128 + (fV*8) + fH;
       Ct = 128 + (tV*8) + tH;
       
       if( ww==1 ) then
          W = W .. ", ";
       else
          ww = 1;
       end

       if(Cf<10) then
         W = W .. string.format("%d",Cf);
       else
         W = W .. "$".. string.format("%x",Cf);
       end
       ou_filb:write( string.char(Cf) );
       W = W .. ", ";

       if(Ct<10) then
         W = W .. string.format("%d",Ct);
       else
         W = W .. "$".. string.format("%x",Ct);
       end
       ou_filb:write( string.char(Ct) );

       if(d>0) then
         W = W .. ",$ff," .. "'" .. g .."' ";
         ou_filb:write( string.char(255) );
         ou_filb:write( g );
       end

       
       cnt = cnt + 1;
       if(cnt==8) then
          ou_file:write( W .. CR );
          cnt = 0;
          ww=0;
          W="";
       end
 
       wm = 1-wm;

     end

     if(cnt == 0) then
         ou_file:write("   .by 0" .. CR .. CR);
     else
         ou_file:write(W .. ",0" .. CR .. CR);
     end

     ou_filb:write(string.char(0));

     print("Moves " .. Mn);

     print(I);
   end
 end
end

ou_file:write(CR .. "   .by 0, 0, 0, 0" .. CR .. CR);
ou_filb:write(string.char(0));
ou_filb:write(string.char(0));
ou_filb:write(string.char(0));
ou_filb:write(string.char(0));

in_file:close();
ou_file:close();
ou_filb:close();
print("Data prepared, Ok"); 

-- Call samples...
--c0_LuaChess.a_SAMPLES ()
