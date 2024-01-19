###################  HTML variables ################
checkbox_tag <- '<div class="pretty p-default p-switch p-fill ">
                <input id="y-bool_perc_users" type="checkbox" class="shiny-bound-input shinyjs-resettable" data-shinyjs-resettable-id="y-bool_perc_users" data-shinyjs-resettable-type="Checkbox" data-shinyjs-resettable-value="True" checked >
                <div class="state p-success">
                  <label>
                    
                  </label>
                </div>
              </div>'
file_Structure_content <- '
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>File Structure</title>
        <style>
            /* Add some basic styling for better presentation */
            ul {
                list-style-type: none;
                padding: 0;
                margin: 0;
            }

            li {
                margin-bottom: 5px;
            }

            /* Style folder and file icons */
            .folder {
                margin-right: 5px;
                color: #007BFF;
            }

            .file {
                margin-right: 5px;
                color: #6C757D;
            }

            .compressed-folder {
                margin-right: 5px;
                color: #28a745;
            }

            .excel-file {
                margin-right: 5px;
                color: #1f73b7;
            }

            .subfolder {
                margin-left: 20px;
            }

            .line {
                border-bottom: 1px solid #ced4da;
            }
        </style>
    </head>
    <body>
        <ul>
            <!-- Root folder -->
            <li class="line">
                <span class="compressed-folder">🗜️️</span> Zip file structure
                <ul class="subfolder">
                    <!-- Subfolder 1 -->
                    <li class="line">
                        <span class="folder">📁</span> txt
                        <ul class="subfolder">
                            <!-- File 1.1.1 -->
                            <li class="line">
                                <span class="file">📄</span> document1_id.txt
                            </li>
                            <!-- File 1.1.2 -->
                            <li><span class="file">📄</span> document2_id.txt</li>
                            <li><span class="file">📄</span> ...</li>
                            <li><span class="file">📄</span> documentN_id.txt</li>
                        </ul>
                    </li>
                    <!-- Excel file -->
                    <li class="line">
                        <span class="excel-file">📑</span> mention_file.tsv
                    </li>
                </ul>
            </li>
        </ul>

    </body>
    </html>
  '
table_codes <- '
  <!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <style>
    table {
      border-collapse: collapse;
      width: 100%;
    }

    th, td {
      border: 1px solid #ddd;
      padding: 8px;
      text-align: left;
    }

    th {
      background-color: #f2f2f2;
    }
  </style>
</head>
<body>
  <table>
    <thead>
      <tr>
        <th>filenameid</th>
        <th>mention_class</th>
        <th>span</th>
        <th>codes</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>document1_id#579#606</td>
        <td>SINTOMA</td>
        <td>segmentos estaban alineados</td>
        <td>[399898009, 5671000124104, 249962002, 422949007]</td>
      </tr>
      <!-- Agrega más filas según sea necesario -->
    </tbody>
  </table>

</body>
</html>  '
table_no_codes <- '
  <!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <style>
    table {
      border-collapse: collapse;
      width: 100%;
    }

    th, td {
      border: 1px solid #ddd;
      padding: 8px;
      text-align: left;
    }

    th {
      background-color: #f2f2f2;
    }
  </style>
</head>
<body>

  <table>
    <thead>
      <tr>
        <th>filenameid</th>
        <th>mention_class</th>
        <th>span</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>document1_id#579#606</td>
        <td>SINTOMA</td>
        <td>segmentos estaban alineados</td>
      </tr>
      <!-- Agrega más filas según sea necesario -->
    </tbody>
  </table>

</body>
</html> 
  '
table_gazz <- '
  <!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <style>
    table {
      border-collapse: collapse;
      width: 100%;
    }

    th, td {
      border: 1px solid #ddd;
      padding: 8px;
      text-align: left;
    }

    th {
      background-color: #f2f2f2;
    }
  </style>
</head>
<body>
  <table>
    <thead>
      <tr>
        <th>code</th>
        <th>language</th>
        <th>term</th>
        <th>semantic_tag</th>
        <th>mainterm</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>9993006</td>
        <td>es</td>
        <td>incisión del diafragma</td>
        <td>procedure</td>
        <td>1</td>
      </tr>
      <tr>
        <td>9990009</td>
        <td>es</td>
        <td>servicio de planificación del tratamiento de radioterapia</td>
        <td>procedure</td>
        <td>1</td>
      </tr>
      <tr>
        <td>9990009</td>
        <td>es</td>
        <td>servicio de planificación del tratamiento de terapia radiante</td>
        <td>procedure</td>
        <td>0</td>
      </tr>
      <!-- Agrega más filas según sea necesario -->
    </tbody>
  </table>

</body>
</html>  '
################# Functions ####################
help_projectcreation <- function(){
    tagList(
        box(title = "Project Creation Interface help", status = "primary", solidHeader = TRUE,width=12,
            collapsible = TRUE, collapsed = FALSE, 
            # Step 1: Enter Name and Description
            h4("1. Define a Project Name and Description"),
            tags$ol(
                div(HTML(("1.1. In the <em>Project name</em> field, enter a descriptive name for the data you are uploading."))),
                p("1.2. Provide a brief description that identifies the content or purpose of the data to be normalised.")
            ),
            
            
            # Step 2: Upload Files and Click "Upload" Button
            h4("2. Upload Project Data"),
            tags$ol(
                div(HTML("2.1. Click the <button type='button' disabled>Browse...</button> button to open your device's file explorer.")),
                div(HTML("2.2. Choose the data you want to upload compressed as a <em>*.zip</em> file. More information on the data format can be found in the 'Data format' help section. ")),
                div(HTML("2.3. Indicate whether the uploaded data has candidate codes associated with it or not using the checkbox (<input type='checkbox' checked>).")),
                p("2.4. Select the gazetteer you want to use to normalize the data using the Search dropdown"),
                div(HTML("2.5. To complete the uploading proccess, press the <button type='button' disabled>Upload data</button> to upload tha data to the server.")),
                p("2.6. Fix errors in the upload process that may have arisen.."),
            ),
            
            
            # Step 3: Configure Data Distribution
            h4("3. Select annotators to normalize the data"),
            tags$ol(
                div(HTML("3.1. Select the users that will annotate the data using the <em>Search and select users</em> dropdown. Create a new user in the <em>Users</em> menu of the application in advance if required")),
                div(HTML(paste0("3.2. Choose whether the project will have a percentage of documents for inter-annotator agreement computation (",checkbox_tag,"). If required, choose the percentage of documents that will be assigned to all annotators in the project to calculate the agreement index."))),
                div(HTML(paste0("3.3. If you want to assign specific percentage of documents to each user, indicate it in the interface (",checkbox_tag,") and assign the percentage of documents to each one with the input sliders (<input type='range' style='width: 15%; display: inline-block;' disabled>). The percentage assigned must sum to 100%. Note that in this case the documents assigned to each annotator will be different."))),
                div(HTML("3.4. To complete the project creation process, press <button type='button' disabled>Create new project</button> and wait for the project to be created without reloading the page")),
                div(HTML("3.5. To view the project in the side menu you must reload the application URL")),
                
            )
            ),
        box(title = "Data format help", status = "primary", solidHeader = TRUE,width=12,
            collapsible = TRUE, collapsed = TRUE,
            h4("1. Zip structure"),
            p("The uploaded data file must be compressed in zip format and must have the structure shown in the image, where:"),
            tags$ol(
                tags$li(HTML("<b>'txt' folder</b> contains the list of documents from which the mentions to be standardized have been extracted. Each document will be in a separate txt file, named with the document identifier.")),
                tags$li(HTML("<b>'mentions_file.tsv'</b> is a tab-separated file that must contain certain specific columns. This column structure is explained in point 2."))
            ),
            HTML(file_Structure_content),
            tags$br(),
            HTML("<h4>2. Structure of <em>mentions_file.tsv</em></h4>"),
            p("Depending on whether you want to generate candidates using the tool, or the candidates are pre-calculated, different structures will be required:"),
            tags$ol(
                tags$li(
                    tagList(
                        HTML('<b>The data has pre-calculated candidate codes</b>: In this case it is required that there are 4 columns called "filenameid", which concatenates the id of the document from which the mention has been extracted together with the initial and final character of the mention within the text joined by the symbol "#"; "mention_class", which represents the category of the extracted mention; "span", corresponding to the text extraction of the mention; and "codes", which is a list of identifiers of candidate concepts associated to the mention calculated with an external method.'),
                        HTML(table_codes)
                    )
                ),
                tags$li(
                    tagList(
                        HTML('<b>The data has not pre-calculated candidate codes</b>: In this case it is required that there are 3 columns called "filenameid", which concatenates the id of the document from which the mention has been extracted together with the initial and final character of the mention within the text joined by the symbol "#"; "mention_class", which represents the category of the extracted mention; and "span", corresponding to the text extraction of the mention.'),
                        HTML(table_no_codes)
                    )
                )
            ),
            p("")
            )
    )
}
    

help_gazcreation <- function(){
    tagList(
        box(title = "Gazetteer Creation Interface help", status = "primary", solidHeader = TRUE,width=12,
            collapsible = TRUE, collapsed = FALSE, 
            # Step 1: Enter Name and Description
            h4("1. Define a Gazetteer Name and Description"),
            tags$ol(
                div(HTML(("1.1. In the <em>Gazetteer name</em> field, enter a descriptive name for the gazetteer you are uploading."))),
                p("1.2. Provide a brief description that identifies the content or purpose of the gazetteer to be uploaded.")
            ),
            
            
            # Step 2: Upload Files and Click "Upload" Button
            h4("2. Upload Gazetteer Data"),
            tags$ol(
                div(HTML("2.1. Click the <button type='button' disabled>Browse...</button> button to open your device's file explorer.")),
                div(HTML("2.2. Choose the data you want to upload compressed as a <em>*.tsv</em> file. More information on the gazetteer format can be found in the 'Gazetteer format' help section. ")),
                div(HTML("2.3. In case you want to link the terminology concepts to an external online viewer, paste in the textual input a URL pattern indicating where the code would go with the string '{CODE}'.")),
                div(HTML("2.4. Press <button type='button' disabled>Upload</button> to upload tha gazetteer to the server.")),
                div(HTML("2.5. To complete the gazetteer creation process, press <button type='button' disabled>Create new gazetteer</button> and wait for the gazetteer to be created without reloading the page")),
                p("2.6. Fix errors in the loading process that may have arisen."),
                div(HTML("2.7. To view the project in the side menu you must reload the application URL")),
                
            ),

        ),
        box(title = "Gazetteer format help", status = "primary", solidHeader = TRUE,width=12,
            collapsible = TRUE, collapsed = TRUE,
            h4("1. TSV structure"),
            # "El archivo de gazetteer subido debe estar en formato tsv (archivo de columnas separadas por tabulaciones), y debe tener de las siguientes columnas con nombre: "
          
            p("The uploaded gazetteer file must be in tsv format (tab separated column file). Each row in the file represents a lexical entry in the gazetteer and must have the following named columns: "),
            tags$ol(
                tags$li(
                    HTML('<b><em>code</em></b>: Terminology code associated with the term.')
                ),
                tags$li(
                    HTML('<b><em>language</em></b>: Language of lexical entry')
                ),
                tags$li(
                    HTML('<b><em>term</em></b>: Text of the term')
                ),
                tags$li(
                    HTML('<b><em>semantic_tag</em></b>: Semantic label of the term. For example disease, procedure, symptom...')
                ),
                tags$li(
                    HTML('<b><em>mainterm</em></b>: This column indicates whether the term corresponds to the preferred term of the associated concept (value 1) or is synonymous (value 0).')
                )
            ),
            p("NOTE: In the uploaded file it is necessary that for each code there is at least one mainterm (a row with the column 'mainterm' with value 1). If you do not know the semantic tag you can fill the field with the value 'default'."),
            HTML(table_gazz)
            ),
        )
    
}
    
    