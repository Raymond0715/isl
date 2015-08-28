package isl;

import java.lang.ref.PhantomReference;
import java.lang.ref.ReferenceQueue;

abstract class DLnkdPhntmRef extends PhantomReference<Object> {

    private DLnkdPhntmRef prv = null;
    private DLnkdPhntmRef nxt = null;

    long ptr = 0;

    public DLnkdPhntmRef(Object referent, ReferenceQueue<? super Object> refQ) {
        super(referent, refQ);
    }

    final void insertAfter(DLnkdPhntmRef newPrev) {
        this.prv = newPrev;
        this.nxt = newPrev.nxt;
        this.prv.nxt = this;
        this.nxt.prv = this;
    }

    final void setPointer(long ptr) {
        this.ptr = ptr;
    }

    final void remove() {
        this.prv.nxt = this.nxt;
        this.nxt.prv = this.prv;
        this.nxt = null;
        this.prv = null;
    }

    abstract void freeCPtr();

    static final DLnkdPhntmRef createListDelims() {
        DLnkdPhntmRef start = new DummyDLnkdPhntmRef();
        DLnkdPhntmRef end = new DummyDLnkdPhntmRef();
        start.nxt = end;
        end.prv = start;
        return start;
    }

    private static final class DummyDLnkdPhntmRef extends DLnkdPhntmRef {
        private DummyDLnkdPhntmRef() {
            super(new Object(), null);
        }

        final void freeCPtr() {
            throw new InternalError("Oops, dummy references must not be freed!");
        }
    }
}
