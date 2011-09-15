
task :default => :usage

task :usage do
  puts 'Try one of these commands:'
  sh %{rake -T}
end

desc 'Performs clean build.'
task :cleanbuild => [:clean,
                     :config,
                     :build]

desc 'Configures the project for development.'
task :config do
  sh %{cabal configure -f development --enable-tests}
end

desc 'Cleans up everything.'
task :clean do
  sh %{cabal clean}
end

desc 'Builds everything.'
task :build do
  sh %{cabal build}
end

desc 'Tests.'
task :test => :build do
  sh %{cabal test}
end

desc 'Runs bakers12.'
task :run, [:args] => :build do |t, args|
  args.with_default(:args => '')
  sh %{./dist/build/bakers12/bakers12 #{args[:args]}}
end

namespace :release do

  desc 'Cleans up everything and configures for release.'
  task :build => [:clean, 'release:config'] do
    sh %{cabal build}
  end

  desc 'Configures the project for development.'
  task :config do
    sh %{cabal configure}
  end

end


