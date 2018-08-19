require 'rake/clean'
CLEAN.include('*.fasl', '*~')
CLOBBER.include('sudoku','sudoku-vector')
              
task :default => ["sudoku"]

desc "Build all sudoku solvers"

task :all => ['sudoku', 'sudoku-vector']

desc "Build the sudoku application"

file :sudoku => %w[sudoku.lisp  auxfns.lisp  sudoku.asd] do |t|
  command = "buildapp --compress-core --asdf-tree #{ENV['HOME']}/quicklisp/dists/quicklisp/software/   --load-system #{t.name} --entry main --output #{t.name}"
#  puts "command: #{command}"
  sh command
end


desc "Build the sudoku-vector application"
file 'sudoku-vector' => %w[sudoku-vector.lisp  auxfns.lisp  sudoku.asd] do |t|
  command = "buildapp --compress-core --asdf-tree #{ENV['HOME']}/quicklisp/dists/quicklisp/software/ --load-system #{t.name} --entry main --output #{t.name}"
  #puts "Executing: #{command}"
  sh command
end

desc "Run sudoku on the fiend5402.txt data"
task :test => [:sudoku] do
  sh "./sudoku fiend5402.txt"
end
