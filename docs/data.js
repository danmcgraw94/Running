// Function to display training data on page
function displayTrainingData(data) {
  const container = document.getElementById('training-container');
  container.innerHTML = ''; // Clear existing content

  data.forEach(row => {
    const div = document.createElement('div');
    div.textContent = `${row.date} (${row.day}): ${row.workout} — ${row.miles} miles`;
    container.appendChild(div);
  });
}

// Load CSV and parse it
fetch('Training_This_Week.csv')
  .then(response => response.text())
  .then(csvText => {
    const results = Papa.parse(csvText, {
      header: true,
      skipEmptyLines: true,
    });
    // results.data is array of objects with keys from CSV header
    displayTrainingData(results.data);
  })
  .catch(err => console.error('Error loading CSV:', err));
