import matplotlib.pyplot as plt
import numpy as np
import time

# Generate sample data
x = np.linspace(0, 10, 100)
y = np.sin(x)

# Create figure
plt.figure(figsize=(8, 6))
plt.plot(x, y)
plt.title('Sample Plot')
plt.xlabel('X axis')
plt.ylabel('Y axis')

# Save figure
plt.savefig('sample_plot.jpg')

# Add delay
time.sleep(1)

# Show plot
plt.show()
