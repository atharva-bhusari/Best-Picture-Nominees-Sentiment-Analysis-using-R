library(pdftools)

# Specify the URL of the PDF file
pdf_url = "url for the script pdf"

# Download the PDF file and save it to a temporary location
temp_file = tempfile(fileext = ".pdf")
download.file(pdf_url, temp_file, mode = "wb")

# Read the PDF file and extract text
pdf = pdf_text(temp_file)

pdf_df = tibble(line = character(0))

for (i in seq_along(pdf)) {
  # Split the text into lines
  lines = strsplit(pdf[i], "\n")[[1]]
  # Create a tibble for the lines on the current page
  page_df = tibble(line = lines)
  # Append the page_df to the pdf_df
  pdf_df = bind_rows(pdf_df, page_df)
}