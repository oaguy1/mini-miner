-- Load fennel
fennel = require("lib.fennel")
wf = require("lib.windfield")

-- Use fennel's tracebacks, makes things intelligeble
debug.traceback = fennel.traceback

-- Allow require to load fennel files
table.insert(package.loaders, function(filename)
   if love.filesystem.getInfo(filename) then
      return function(...)
         return fennel.eval(love.filesystem.read(filename), {env=_G, filename=filename}, ...), filename
      end
   end
end)

-- Load main fennel file
require("main.fnl")
