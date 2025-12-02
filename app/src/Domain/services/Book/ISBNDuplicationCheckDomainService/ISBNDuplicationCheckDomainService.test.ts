import { ISBNDuplicationCheckDomainService } from './ISBNDuplicationCheckDomainService';
import { BookId } from 'Domain/models/Book/BookId/BookId';

describe('ISBNDuplicationCheckDomainService', () => {
  it('重複していない場合はfalseを返す', async () => {
    const service = new ISBNDuplicationCheckDomainService();
    const bookId = new BookId('9784167158057');
    const result = await service.execute(bookId);

    expect(result).toBe(false);
  });
});
