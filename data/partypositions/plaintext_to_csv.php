<?php

// Get all txt files in __DIR__ . "/plaintext"
$files = glob(__DIR__ . "/plaintext/*.txt");

$data = [
    [
        "id",
        "text",
        "code",
        "party"
    ]
];

$id = 1;

// Iterate over each file and split the conent by double linebreaks
foreach($files as $file) {
    $paragraphs = plaintext_to_array($file);

    // Split filename by underscores
    $filename = basename($file, '.txt');
    $filename_parts = explode("_", $filename);

    // Iterate over each paragraph and append the data to the $data array
    foreach($paragraphs as $key => $paragraph) {
        $data[] = [
            "id" => $id++,
            "text" => $paragraph,
            "code" => $filename_parts[0],
            "party" => $filename_parts[1]
        ];
    }
}

// Create or overwrite the file "data/partypositions/partypositions.csv"
$fp = fopen(__DIR__ . "/partypositions.csv", "w");
foreach($data as $fields) {
    fputcsv($fp, $fields);
}
fclose($fp);

/**
 * Split the content of a txt file by double linebreaks and return an array
 *
 * @param string $file
 * @return array
 */
function plaintext_to_array($file) {
    // Open the file
    $handle = fopen($file, "r");

    // Read the content
    $content = fread($handle, filesize($file));

    // Split the content by double linebreaks
    $data = array_map('trim', explode("\n\n", $content));

    // Check if any of the paragraphs contain a single linebreak
    foreach($data as $key => $paragraph) {
        if (strpos($paragraph, "\n") !== false) {
            echo("The paragraph with the key $key in the file $file contains a single linebreak. Please fix this.");
            echo("\n");
            echo($paragraph);
            die();
        }
    }

    fclose($handle);

    return $data;
}