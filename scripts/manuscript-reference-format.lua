-- Preserve the author's regular-type volume insertion in the October 16
-- tracked manuscript. Resolve citations first, keeping other APA styling intact.
-- manuscript.qmd disables automatic citeproc to avoid a second styling pass.
function Pandoc(document)
  document = pandoc.utils.citeproc(document)
  -- The Typst writer would otherwise resolve Cite nodes natively and append
  -- another bibliography. Keep the already-resolved text and link targets.
  document.meta.bibliography = nil
  document.meta.references = nil
  document.meta.nocite = nil
  return document:walk({
    Cite = function(citation) return citation.content end,
    Div = function(div)
      if div.identifier ~= "ref-sharma2005" then return end
      return div:walk({
        Emph = function(emphasis)
          if pandoc.utils.stringify(emphasis) == "89" then
            return emphasis.content
          end
        end
      })
    end
  })
end
