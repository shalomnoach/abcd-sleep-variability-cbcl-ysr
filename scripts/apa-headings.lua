-- Keep APA levels 4 and 5 in the same paragraph as their following text.
function Meta(meta)
  -- The DOCX title page is authored explicitly in manuscript.qmd so it can
  -- match the PDF title page instead of Pandoc's generic title block.
  if FORMAT:match("docx") then
    meta.title = nil
    meta.author = nil
    meta.affiliations = nil
  end
  return meta
end

function Div(div)
  if FORMAT:match("typst") and div.attributes["custom-style"] == "Abstract" then
    -- Libertinus is wider than the submission Word font. A 23pt baseline
    -- keeps the complete 12pt abstract and its abbreviations on one page.
    div.content:insert(1, pandoc.RawBlock("typst",
      "#set par(leading: 11pt, first-line-indent: (amount: 0.5in, all: true))"))
    return div
  end
end

function Para(paragraph)
  if not FORMAT:match("typst") then return end
  local text = pandoc.utils.stringify(paragraph):gsub("’", "'")
  if text:match("^What's known%?$") or text:match("^What's new%?$")
      or text:match("^What's relevant%?$")
      or text:match("^Conflicts of interest:") or text:match("^Correspondence:") then
    return {pandoc.RawBlock("typst", "#block[\n#set par(first-line-indent: 0pt)"),
      paragraph, pandoc.RawBlock("typst", "]")}
  end
end

function Blocks(blocks)
  if not FORMAT:match("typst") then return end
  local result = pandoc.List()
  local i = 1
  while i <= #blocks do
    local heading = blocks[i]
    local following = blocks[i + 1]
    if heading.t == "Header" and (heading.level == 4 or heading.level == 5)
        and following and following.t == "Para" then
      local title = pandoc.Inlines(heading.content)
      if not pandoc.utils.stringify(title):match("%.$") then
        title:insert(pandoc.Str("."))
      end
      local styled = heading.level == 5 and pandoc.Emph(title) or title
      local content = pandoc.Inlines({
        pandoc.Span({pandoc.Strong(styled)}, heading.attr), pandoc.Space()
      })
      content:extend(following.content)
      result:insert(pandoc.Para(content))
      i = i + 2
    else
      result:insert(heading)
      i = i + 1
    end
  end
  return result
end
