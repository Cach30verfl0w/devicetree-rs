use crate::Error;

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub struct Stack<T, const MAX: usize> {
    data: [T; MAX],
    top: usize
}

impl<T: Default + Copy, const MAX: usize> Default for Stack<T, MAX> {
    fn default() -> Self {
        Self {
            data: [T::default(); MAX],
            top: 0
        }
    }
}

impl<T, const MAX: usize> Stack<T, MAX> {
    pub fn push(&mut self, value: T) -> Result<(), Error> {
        if self.top >= MAX {
            return Err(Error::StackOverflow);
        }
        
        self.data[self.top] = value;
        self.top += 1;
        Ok(())
    }
    
    pub fn pop(&mut self) -> Option<&T> {
        if self.top == 0 {
            return None;
        }
        
        self.top -= 1;
        Some(&self.data[self.top])
    }
    
    pub fn top(&self) -> Option<&T> {
        if self.top == 0 {
            return None;
        }
        
        Some(&self.data[self.top - 1])
    }
}
