#!/bin/ruby
# Przykladowy plik konwertujacy
INFILE = 'jensen.sxt'
OUTFILE1 = INFILE.sub('.sxt', '.dmp')
OUTFILE2 = INFILE.sub('.sxt', '.hdmp')
File.delete(INFILE) if File.exist?(INFILE)
File.delete(OUTFILE1) if File.exist?(OUTFILE1)
File.delete(OUTFILE2) if File.exist?(OUTFILE2)

FIB = <<EOF
program Fib
 procedure fib(value i, value index)
  begin
    if i < 2 then
      return 1
    fi;
    return fib(i-2, index+1) + fib(i-1, index+1)
  end
local n
begin
  read n;
  write fib(n, 0)
end
EOF

JENSEN = <<EOF
program Jensen
 procedure sum(expr, index)
  local result
  begin
    write 0102;
    result := 0;
    while index > 0 do
      write index;
      result := result + expr;
      index := index -1
    done;
    return result
  end
local n
begin
  write 77;
  n := 5;
  write sum( n*n+1, n)
end
EOF

SIMPLE = <<EOF
program simple
local n
begin
  n := 10;
  write n;
  write n+2
end
EOF

PROGRAM = SIMPLE.lines.map(&:strip).join(' ')
PROLOG_PROGRAM = "atom_codes(\"#{PROGRAM}\", X), algol16(X,E), write(E)"
`swipl -s compiler.pl -t '#{PROLOG_PROGRAM}' -q > #{INFILE}`

class Integer
  def to_hex
    to_s(16).rjust(4, '0')
  end
end

nums = File.read(INFILE)
            .gsub('[', '')
            .gsub(']', '')
            .strip
            .split(',')
            .map(&:to_i)

File.write(OUTFILE1, nums.map(&:to_hex).join("\n"))
`cat #{OUTFILE1} | xxd -p -r > #{OUTFILE2}`

File.delete(INFILE) if File.exist?(INFILE)
File.delete(OUTFILE1) if File.exist?(OUTFILE1)
