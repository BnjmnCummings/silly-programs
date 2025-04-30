# A simple heap copying garbage collector implementation.
# How does this work? - it's kind of like seperating an egg yolk from an egg white (hear me out)
#
# 1. We partition the heap into two spaces: from_space and to_space.
#    - from_space is where we allocate new objects.
#    - we keep the the to_space empty.
#
# 2. The garbage collector will copy all reachable objects from the from_space to the to_space.
#    - it will also update the references of the copied objects to point to their new locations in the to_space.
#
# 3. The from_space is then cleared, and the to_space becomes the new from_space.
#
# So, every time the garbage collector runs, we migrate all of the relevant/reachable objects to a new space, 
# and completely wipe the old space. Cleaning away all of the dead objects.
# As you can imagine, this is a very efficient way to clean up the heap. However, 
# we are cutting the avaiable heap size in half. But we don't have to worry about fragmentation or mess about with pointer two much.

class Object:
    def __init__(self, name):
        self.name = name
        self.references = []
        self.forwarded = False
        self.new_location = None

    def __repr__(self):
        return f"Object({self.name})"

class CopyingGC:
    def __init__(self, heap_size):
        self.heap_size = heap_size
        self.from_space = [None] * heap_size
        self.to_space = [None] * heap_size
        self.next_free = 0
        self.roots = []
        
    def allocate(self, name):
        if self.next_free >= len(self.from_space):
            print("\n[GC Triggered] Heap full, starting collection...")
            self.collect()
            if self.next_free >= len(self.from_space):
                raise MemoryError("Out of memory after GC")
        
        obj = Object(name)
        self.from_space[self.next_free] = obj
        self.next_free += 1
        print(f"Allocated {obj}")
        return obj
    
    def add_reference(self, obj, referenced_obj):
        if referenced_obj is not None:
            obj.references.append(referenced_obj)
            print(f"Added reference: {obj.name} -> {referenced_obj.name}")
    
    def add_root(self, obj):
        if obj is not None:
            self.roots.append(obj)
            print(f"Added root: {obj.name}")
    
    def collect(self):
        # Swap spaces
        self.from_space, self.to_space = self.to_space, self.from_space
        self.next_free = 0
        
        # Copy roots and their reachable objects
        for root in self.roots:
            if root and not root.forwarded:
                self.copy(root)
        
        # Update references
        for i in range(self.next_free):
            obj = self.to_space[i]
            if obj:
                obj.references = [ref.new_location for ref in obj.references if ref]
        
        # Reset forwarding info
        for i in range(self.next_free):
            if self.to_space[i]:
                self.to_space[i].forwarded = False
                self.to_space[i].new_location = None
        
        print(f"[GC Complete] {self.next_free} objects survived collection")
    
    def copy(self, obj):
        if obj is None:
            return None
            
        if obj.forwarded:
            return obj.new_location
        
        # Copy object
        new_obj = Object(obj.name)
        new_obj.references = [ref for ref in obj.references if ref is not None]
        
        if self.next_free >= len(self.to_space):
            raise MemoryError("Out of memory during collection")
        
        self.to_space[self.next_free] = new_obj
        obj.forwarded = True
        obj.new_location = new_obj
        self.next_free += 1
        
        # Copy all referenced objects
        new_obj.references = [self.copy(ref) for ref in new_obj.references]
        
        print(f"  Copied {obj.name} to new space")
        return new_obj

    def print_heap(self, space_name):
        space = self.from_space if space_name == "from" else self.to_space
        print(f"\n{space_name.upper()} SPACE:")
        for i, obj in enumerate(space):
            status = ""
            if obj:
                refs = [r.name for r in obj.references if r]
                status = f"{i}: {obj.name} -> {refs}"
                if hasattr(obj, 'forwarded') and obj.forwarded:
                    status += " (forwarded)"
            elif i < self.next_free:
                status = f"{i}: Empty slot"
            if status:
                print(status)

# Example Usage
print("=== INITIALIZING GC WITH HEAP SIZE 5 ===")
gc = CopyingGC(heap_size=5)

print("\n=== PHASE 1: ALLOCATING OBJECTS ===")
a = gc.allocate("A")
b = gc.allocate("B")
c = gc.allocate("C")
gc.add_root(a)
gc.add_reference(a, b)
gc.add_reference(b, c)

print("\n=== PHASE 2: CREATING GARBAGE ===")
d = gc.allocate("D")  # This will be garbage
gc.add_reference(b, d) 

print("\n=== PHASE 3: TRIGGERING GC ===")
# This allocation will trigger GC since heap is full
e = gc.allocate("E")  

print("\n=== PHASE 4: POST-GC STATE ===")
gc.print_heap("from")  # Shows old space with forwarded objects
gc.print_heap("to")    # Shows new space with live objects

print("\n=== PHASE 5: ALLOCATING MORE OBJECTS ===")
f = gc.allocate("F")
gc.add_reference(e, f)
gc.print_heap("to")
