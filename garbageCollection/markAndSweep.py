# This is a simple implementation of the Mark and Sweep garbage collection algorithm.
# We perform a dfs or equivalent traversal to mark all reachable objects from the root set.
# After marking, we sweep through the heap to collect all unmarked objects and bin them (free the memory).
# The 'root set' will be a list of objects that are directly accessible (like global variables or stack variables).

class Object:
    def __init__(self, name):
        self.name = name
        self.references = []
        self.marked = False

    def add_reference(self, obj):
        self.references.append(obj)

class Heap:
    def __init__(self):
        self.objects = []

    def allocate(self, name):
        obj = Object(name)
        self.objects.append(obj)
        return obj

class GarbageCollector:
    def __init__(self, heap):
        self.heap = heap

    def mark(self, obj):
        if obj.marked:
            return  # Already marked
        obj.marked = True
        for ref in obj.references:
            self.mark(ref)  # Recursively mark references

    def sweep(self):
        new_objects = []
        for obj in self.heap.objects:
            if obj.marked:
                obj.marked = False  # Reset for next GC
                new_objects.append(obj)
            else:
                print(f"Collected garbage: {obj.name}")
        self.heap.objects = new_objects

    def collect(self, roots):
        print("\n--- Starting Garbage Collection ---")
        # Mark phase
        for root in roots:
            self.mark(root)
        # Sweep phase
        self.sweep()

# --- Example Usage ---
heap = Heap()
# Allocate objects
a = heap.allocate("A")
b = heap.allocate("B")
c = heap.allocate("C")
d = heap.allocate("D")

# Build a reference graph (A -> B -> C; D is unreachable)
a.add_reference(b)
b.add_reference(c)

# Run GC (roots = [a])
gc = GarbageCollector(heap)
gc.collect(roots=[a])

# After GC: A, B, C remain; D is collected
print("\nObjects left in heap:")
for obj in heap.objects:
    print(obj.name)
