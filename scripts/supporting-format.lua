-- Center supporting-information equations with room between multiline rows.
-- DOCX custom styles are completed by docx_postprocess.R.
function Div(div)
  local style = div.attributes["custom-style"]
  if not FORMAT:match("typst") or
      (style ~= "SupportingEquation" and style ~= "SupportingEquationSmall") then
    return
  end
  local size = style == "SupportingEquationSmall" and 11 or 12
  local result = {
    pandoc.RawBlock("typst", "#block(width: 100%, above: 6pt, below: 6pt)[\n"
      .. "#set align(center)\n"
      .. "#set text(size: " .. size .. "pt)\n"
      .. "#set par(first-line-indent: 0pt, leading: 1.25em)\n"
      .. "#show math.equation.where(block: true): set block(above: 0pt, below: 0pt)")
  }
  for _, block in ipairs(div.content) do result[#result + 1] = block end
  result[#result + 1] = pandoc.RawBlock("typst", "]")
  return result
end
