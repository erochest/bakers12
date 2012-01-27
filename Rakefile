
COFFEE = [
  'bakers12/static/js/script.coffee',
]

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
task :config, [:target] do |t, args|
  target = args[:target] || 'development'

  case target
  when 'development'
    flags = %{-f development --enable-tests}
  when 'profiling'
    flags = %{-f profiling --enable-library-profiling --ghc-option=-auto-all}
  when 'production'
    flags = %{--ghc-option=-dno-debug-output}
  else
    flags = ''
  end

  sh %{cabal configure #{flags}}
end

desc 'Cleans up everything.'
task :clean do
  sh %{cabal clean}
end

desc 'Builds everything.'
task :build => ['coffee:build',
                'compass:build',
                'hs:build']

desc 'Tests.'
task :test => :build do
  sh %{cabal test}
end

require 'zayin/rake/docco'
desc 'This runs docco to create documentation.'
Zayin::Rake::docco :docco, Dir.glob(File.join('lib', '**', '*.hs')) + Dir.glob(File.join('src', '**', '*.hs'))

desc 'Creates the docs and commits into the gh-pages branch.'
task :ghpages, [:msg] => [:docco] do |t, args|
  msg = args[:msg] || 'Updated docs.'

  branch = `git branch | grep '*' | cut -f2 -d' '`

  FileUtils.rmtree('/tmp/bakers12', :verbose => true) if File.exists?('/tmp/bakers12')
  FileUtils.mv('docs', '/tmp/bakers12', :verbose => true)

  sh %{git stash}
  sh %{git checkout gh-pages}

  FileUtils.rmtree('docs', :verbose => true) if File.exists?('docs')
  FileUtils.mv('/tmp/bakers12', 'docs', :verbose => true)

  sh %{git add --all docs}
  sh %{git commit -m "#{msg}"}

  puts "Don't forget to run these commands:"
  puts "    git checkout #{branch}"
  puts "    git stash pop"
end

desc 'Runs bakers12.'
task :run, [:args] => :build do |t, args|
  args.with_default(:args => '')
  sh %{./dist/build/bakers12/bakers12 #{args[:args]}}
end

namespace :coffee do
  desc 'This watches the CoffeeScript files and recompiles them automatically.'
  task :watch do
    sh %(coffee --watch --compile #{COFFEE.join(' ')})
  end

  desc 'This compiles all the CoffeeScript files.'
  task :build do
    sh %(coffee --compile #{COFFEE.join(' ')})
  end
end

namespace :compass do
  desc 'This watches the SASS files and recompiles them automatically.'
  task :watch do
    sh %(compass watch bakers12/static)
  end

  desc 'This compiles all the SASS files.'
  task :build do
    sh %(compass compile bakers12/static)
  end
end

multitask :watch => ['coffee:watch',
                     'compass:watch']

namespace :hs do
  desc 'This builds the Haskell part of the project.'
  task :build do
    sh %{cabal build}
  end

  desc 'This creates the documentation. (This fails unless the "Executable" section is removed from bakers12.cabal.)'
  task :docs do
    sh %{cabal haddock --hyperlink-source}
  end

  desc 'This runs hlint on the library.'
  task :lint do
    sh %{hlint lib}
  end
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


